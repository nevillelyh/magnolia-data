/*
 * Copyright 2020 Spotify AB.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package magnolify.protobuf

import java.lang.reflect.Method
import java.{util => ju}

import com.google.protobuf.Descriptors.FileDescriptor.Syntax
import com.google.protobuf.Descriptors.{Descriptor, FieldDescriptor}
import com.google.protobuf.{ByteString, Message}
import magnolia._
import magnolify.shared.Converter
import magnolify.shims.FactoryCompat
import magnolify.shims.JavaConverters._

import scala.annotation.implicitNotFound
import scala.collection.concurrent
import scala.language.experimental.macros
import scala.reflect.ClassTag

sealed trait ProtobufType[T, MsgT <: Message] extends Converter[T, MsgT, MsgT] {
  def apply(r: MsgT): T = from(r)
  def apply(t: T): MsgT = to(t)
}

sealed trait ProtobufOption {
  def check(f: ProtobufField.Record[_], syntax: Syntax): Unit
}

object ProtobufOption {
  implicit val proto2Option: ProtobufOption = new ProtobufOption {
    override def check(f: ProtobufField.Record[_], syntax: Syntax): Unit =
      if (f.hasOptional) {
        require(
          syntax == Syntax.PROTO2,
          "Option[T] support is PROTO2 only, " +
            "`import magnolify.protobuf.unsafe.Proto3Option._` to enable PROTO3 support"
        )
      }
  }

  private[protobuf] class Proto3Option extends ProtobufOption {
    override def check(f: ProtobufField.Record[_], syntax: Syntax): Unit = ()
  }
}

object ProtobufType {
  implicit def apply[T, MsgT <: Message](implicit
    f: ProtobufField.Record[T],
    ct: ClassTag[MsgT],
    po: ProtobufOption
  ): ProtobufType[T, MsgT] =
    new ProtobufType[T, MsgT] {
      if (f.hasOptional) {
        val syntax = ct.runtimeClass
          .getMethod("getDescriptor")
          .invoke(null)
          .asInstanceOf[Descriptor]
          .getFile
          .getSyntax
        po.check(f, syntax)
      }

      @transient private var _newBuilder: Method = _
      private def newBuilder: Message.Builder = {
        if (_newBuilder == null) {
          _newBuilder = ct.runtimeClass.getMethod("newBuilder")
        }
        _newBuilder.invoke(null).asInstanceOf[Message.Builder]
      }

      override def from(v: MsgT): T = f.from(v)
      override def to(v: T): MsgT = f.to(v, newBuilder).asInstanceOf[MsgT]
    }
}

sealed trait ProtobufField[T] extends Serializable {
  type FromT
  type ToT

  val hasOptional: Boolean

  def from(v: FromT): T
  def to(v: T, b: Message.Builder): ToT

  def fromAny(v: Any): T = from(v.asInstanceOf[FromT])
}

object ProtobufField {
  trait Aux[T, From, To] extends ProtobufField[T] {
    override type FromT = From
    override type ToT = To
  }

  trait Record[T] extends Aux[T, Message, Message]

  //////////////////////////////////////////////////

  type Typeclass[T] = ProtobufField[T]

  def combine[T](caseClass: CaseClass[Typeclass, T]): Record[T] = new Record[T] {
    // One Record[T] instance may be used for multiple Message types
    private val fieldsCache: concurrent.Map[String, Array[FieldDescriptor]] =
      concurrent.TrieMap.empty
    private def getFields(descriptor: Descriptor): Array[FieldDescriptor] =
      fieldsCache.getOrElseUpdate(
        descriptor.getFullName, {
          val fields = new Array[FieldDescriptor](caseClass.parameters.size)
          caseClass.parameters.foreach(p => fields(p.index) = descriptor.findFieldByName(p.label))
          fields
        }
      )

    override val hasOptional: Boolean = caseClass.parameters.exists(_.typeclass.hasOptional)

    override def from(v: Message): T = {
      val descriptor = v.getDescriptorForType
      val syntax = descriptor.getFile.getSyntax
      val fields = getFields(descriptor)

      caseClass.construct { p =>
        val field = fields(p.index)
        // hasField behaves correctly on PROTO2 optional fields
        val value = if (syntax == Syntax.PROTO2 && field.isOptional && !v.hasField(field)) {
          null
        } else {
          v.getField(field)
        }
        p.typeclass.fromAny(value)
      }
    }

    override def to(v: T, bu: Message.Builder): Message = {
      val fields = getFields(bu.getDescriptorForType)

      caseClass.parameters
        .foldLeft(bu) { (b, p) =>
          val field = fields(p.index)
          val value = if (field.getType == FieldDescriptor.Type.MESSAGE) {
            // nested records
            p.typeclass.to(p.dereference(v), b.newBuilderForField(field))
          } else {
            // non-nested
            p.typeclass.to(p.dereference(v), null)
          }
          if (value == null) b else b.setField(field, value)
        }
        .build()
    }
  }

  @implicitNotFound("Cannot derive ProtobufField for sealed trait")
  private sealed trait Dispatchable[T]
  def dispatch[T: Dispatchable](sealedTrait: SealedTrait[Typeclass, T]): Record[T] = ???

  implicit def gen[T]: Record[T] = macro Magnolia.gen[T]

  //////////////////////////////////////////////////

  def apply[T](implicit f: ProtobufField[T]): ProtobufField[T] = f

  def from[T]: FromWord[T] = new FromWord[T]

  class FromWord[T] {
    def apply[U](f: T => U)(g: U => T)(implicit pf: ProtobufField[T]): ProtobufField[U] =
      new ProtobufField[U] {
        override type FromT = pf.FromT
        override type ToT = pf.ToT
        override val hasOptional: Boolean = pf.hasOptional
        override def from(v: FromT): U = f(pf.from(v))
        override def to(v: U, b: Message.Builder): ToT = pf.to(g(v), null)
      }
  }

  private def aux[T, From, To](f: From => T)(g: T => To): ProtobufField[T] =
    new ProtobufField[T] {
      override type FromT = From
      override type ToT = To
      override val hasOptional: Boolean = false
      override def from(v: FromT): T = f(v)
      override def to(v: T, b: Message.Builder): ToT = g(v)
    }

  private def aux2[T, Repr](f: Repr => T)(g: T => Repr): ProtobufField[T] =
    aux[T, Repr, Repr](f)(g)

  private def id[T]: ProtobufField[T] = aux[T, T, T](identity)(identity)

  implicit val pfBoolean = id[Boolean]
  implicit val pfInt = id[Int]
  implicit val pfLong = id[Long]
  implicit val pfFloat = id[Float]
  implicit val pfDouble = id[Double]
  implicit val pfString = id[String]
  implicit val pfByteString = id[ByteString]
  implicit val pfByteArray = aux2[Array[Byte], ByteString](_.toByteArray)(ByteString.copyFrom)

  implicit def pfOption[T](implicit f: ProtobufField[T]): ProtobufField[Option[T]] =
    new Aux[Option[T], f.FromT, f.ToT] {
      override val hasOptional: Boolean = true
      override def from(v: f.FromT): Option[T] = if (v == null) None else Some(f.from(v))
      override def to(v: Option[T], b: Message.Builder): f.ToT = v match {
        case None    => null.asInstanceOf[f.ToT]
        case Some(x) => f.to(x, b)
      }
    }

  implicit def pfIterable[T, C[_]](implicit
    f: ProtobufField[T],
    ti: C[T] => Iterable[T],
    fc: FactoryCompat[T, C[T]]
  ): ProtobufField[C[T]] =
    new Aux[C[T], ju.List[f.FromT], ju.List[f.ToT]] {
      override val hasOptional: Boolean = false
      override def from(v: ju.List[f.FromT]): C[T] =
        if (v == null) {
          fc.newBuilder.result()
        } else {
          fc.build(v.asScala.iterator.map(f.from))
        }
      override def to(v: C[T], b: Message.Builder): ju.List[f.ToT] =
        if (v.isEmpty) null else v.iterator.map(f.to(_, b)).toList.asJava
    }
}
