/*
 * Copyright 2020 Spotify AB
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package magnolify.protobuf

import java.lang.reflect.Method
import java.{util => ju}

import com.google.protobuf.Descriptors.FileDescriptor.Syntax
import com.google.protobuf.Descriptors.{Descriptor, EnumValueDescriptor, FieldDescriptor}
import com.google.protobuf.{ByteString, Message, ProtocolMessageEnum}
import magnolia1._
import magnolify.shared._
import magnolify.shims.FactoryCompat

import scala.annotation.implicitNotFound
import scala.collection.concurrent
import scala.reflect.ClassTag
import scala.jdk.CollectionConverters._
import scala.collection.compat._
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
  implicit def apply[T: ProtobufField, MsgT <: Message: ClassTag](implicit
    po: ProtobufOption
  ): ProtobufType[T, MsgT] = ProtobufType(CaseMapper.identity)

  def apply[T, MsgT <: Message](cm: CaseMapper)(implicit
    f: ProtobufField[T],
    ct: ClassTag[MsgT],
    po: ProtobufOption
  ): ProtobufType[T, MsgT] = f match {
    case r: ProtobufField.Record[_] =>
      new ProtobufType[T, MsgT] {
        {
          val descriptor = ct.runtimeClass
            .getMethod("getDescriptor")
            .invoke(null)
            .asInstanceOf[Descriptor]
          if (r.hasOptional) {
            po.check(r, descriptor.getFile.getSyntax)
          }
          r.checkDefaults(descriptor)(cm)
        }

        @transient private var _newBuilder: Method = _
        private def newBuilder: Message.Builder = {
          if (_newBuilder == null) {
            _newBuilder = ct.runtimeClass.getMethod("newBuilder")
          }
          _newBuilder.invoke(null).asInstanceOf[Message.Builder]
        }

        private val caseMapper: CaseMapper = cm
        override def from(v: MsgT): T = r.from(v)(caseMapper)
        override def to(v: T): MsgT = r.to(v, newBuilder)(caseMapper).asInstanceOf[MsgT]
      }
    case _ =>
      throw new IllegalArgumentException(s"ProtobufType can only be created from Record. Got $f")
  }
}

sealed trait ProtobufField[T] extends Serializable {
  type FromT
  type ToT

  def hasOptional: Boolean
  def default: Option[T]

  def checkDefaults(descriptor: Descriptor)(cm: CaseMapper): Unit = ()

  def from(v: FromT)(cm: CaseMapper): T
  def to(v: T, b: Message.Builder)(cm: CaseMapper): ToT

  def fromAny(v: Any)(cm: CaseMapper): T = from(v.asInstanceOf[FromT])(cm)
}

object ProtobufField {
  sealed trait Aux[T, From, To] extends ProtobufField[T] {
    override type FromT = From
    override type ToT = To
  }

  sealed trait Record[T] extends Aux[T, Message, Message] {
    override def default: Option[T] = None
  }

  // ////////////////////////////////////////////////

  type Typeclass[T] = ProtobufField[T]

  def join[T](caseClass: CaseClass[Typeclass, T]): ProtobufField[T] = {
    if (caseClass.isValueClass) {
      val p = caseClass.parameters.head
      val tc = p.typeclass
      new ProtobufField[T] {
        override type FromT = tc.FromT
        override type ToT = tc.ToT
        override def hasOptional: Boolean = tc.hasOptional
        override val default: Option[T] = tc.default.map(x => caseClass.construct(_ => x))
        override def from(v: FromT)(cm: CaseMapper): T = caseClass.construct(_ => tc.from(v)(cm))
        override def to(v: T, b: Message.Builder)(cm: CaseMapper): ToT =
          tc.to(p.dereference(v), b)(cm)
      }

    } else {
      new Record[T] {
        // One Record[T] instance may be used for multiple Message types
        @transient private lazy val fieldsCache: concurrent.Map[String, Array[FieldDescriptor]] =
          concurrent.TrieMap.empty

        private def getFields(descriptor: Descriptor)(cm: CaseMapper): Array[FieldDescriptor] =
          fieldsCache.getOrElseUpdate(
            descriptor.getFullName, {
              val fields = new Array[FieldDescriptor](caseClass.parameters.size)
              caseClass.parameters.foreach(p =>
                fields(p.index) = descriptor.findFieldByName(cm.map(p.label))
              )
              fields
            }
          )

        override val hasOptional: Boolean = caseClass.parameters.exists(_.typeclass.hasOptional)

        override def checkDefaults(descriptor: Descriptor)(cm: CaseMapper): Unit = {
          val syntax = descriptor.getFile.getSyntax
          val fields = getFields(descriptor)(cm)
          caseClass.parameters.foreach { p =>
            val field = fields(p.index)
            val protoDefault = if (syntax == Syntax.PROTO2 && field.hasDefaultValue) {
              Some(p.typeclass.fromAny(field.getDefaultValue)(cm))
            } else {
              p.typeclass.default
            }
            p.default.foreach { d =>
              require(
                protoDefault.contains(d),
                s"Default mismatch ${caseClass.typeName.full}#${p.label}: $d != ${protoDefault.orNull}"
              )
            }
            if (field.getType == FieldDescriptor.Type.MESSAGE) {
              p.typeclass.checkDefaults(field.getMessageType)(cm)
            }
          }
        }

        override def from(v: Message)(cm: CaseMapper): T = {
          val descriptor = v.getDescriptorForType
          val syntax = descriptor.getFile.getSyntax
          val fields = getFields(descriptor)(cm)

          caseClass.construct { p =>
            val field = fields(p.index)
            // hasField behaves correctly on PROTO2 optional fields
            val value = if (syntax == Syntax.PROTO2 && field.isOptional && !v.hasField(field)) {
              null
            } else {
              v.getField(field)
            }
            p.typeclass.fromAny(value)(cm)
          }
        }

        override def to(v: T, bu: Message.Builder)(cm: CaseMapper): Message = {
          val fields = getFields(bu.getDescriptorForType)(cm)

          caseClass.parameters
            .foldLeft(bu) { (b, p) =>
              val field = fields(p.index)
              val value = if (field.getType == FieldDescriptor.Type.MESSAGE) {
                // nested records
                p.typeclass.to(p.dereference(v), b.newBuilderForField(field))(cm)
              } else {
                // non-nested
                p.typeclass.to(p.dereference(v), null)(cm)
              }
              if (value == null) b else b.setField(field, value)
            }
            .build()
        }
      }
    }
  }

  @implicitNotFound("Cannot derive ProtobufField for sealed trait")
  private sealed trait Dispatchable[T]
  def split[T: Dispatchable](sealedTrait: SealedTrait[Typeclass, T]): ProtobufField[T] = ???

  implicit def gen[T]: ProtobufField[T] = macro Magnolia.gen[T]

  // ////////////////////////////////////////////////

  def apply[T](implicit f: ProtobufField[T]): ProtobufField[T] = f

  def from[T]: FromWord[T] = new FromWord[T]

  class FromWord[T] {
    def apply[U](f: T => U)(g: U => T)(implicit pf: ProtobufField[T]): ProtobufField[U] =
      new Aux[U, pf.FromT, pf.ToT] {
        override val hasOptional: Boolean = pf.hasOptional
        override val default: Option[U] = pf.default.map(f)
        override def from(v: FromT)(cm: CaseMapper): U = f(pf.from(v)(cm))
        override def to(v: U, b: Message.Builder)(cm: CaseMapper): ToT = pf.to(g(v), null)(cm)
      }
  }

  private def aux[T, From, To](_default: T)(f: From => T)(g: T => To): ProtobufField[T] =
    new Aux[T, From, To] {
      override def hasOptional: Boolean = false
      override val default: Option[T] = Some(_default)
      override def from(v: FromT)(cm: CaseMapper): T = f(v)
      override def to(v: T, b: Message.Builder)(cm: CaseMapper): ToT = g(v)
    }

  private def aux2[T, Repr](_default: T)(f: Repr => T)(g: T => Repr): ProtobufField[T] =
    aux[T, Repr, Repr](_default)(f)(g)

  private def id[T](_default: T): ProtobufField[T] = aux[T, T, T](_default)(identity)(identity)

  implicit val pfBoolean: ProtobufField[Boolean] = id[Boolean](false)
  implicit val pfInt: ProtobufField[Int] = id[Int](0)
  implicit val pfLong: ProtobufField[Long] = id[Long](0L)
  implicit val pfFloat: ProtobufField[Float] = id[Float](0.0f)
  implicit val pfDouble: ProtobufField[Double] = id[Double](0.0)
  implicit val pfString: ProtobufField[String] = id[String]("")
  implicit val pfByteString: ProtobufField[ByteString] = id[ByteString](ByteString.EMPTY)
  implicit val pfByteArray: ProtobufField[Array[Byte]] =
    aux2[Array[Byte], ByteString](Array.emptyByteArray)(_.toByteArray)(ByteString.copyFrom)

  def `enum`[T, E <: Enum[E] with ProtocolMessageEnum](implicit
    et: EnumType[T],
    ct: ClassTag[E]
  ): ProtobufField[T] = {
    val map = ct.runtimeClass
      .getMethod("values")
      .invoke(null)
      .asInstanceOf[Array[E]]
      .map(e => e.name() -> e)
      .toMap
    val default = et.from(map.values.find(_.getNumber == 0).get.name())
    aux2[T, EnumValueDescriptor](default)(e => et.from(e.getName))(e =>
      map(et.to(e)).getValueDescriptor
    )
  }

  implicit def pfOption[T](implicit f: ProtobufField[T]): ProtobufField[Option[T]] =
    new Aux[Option[T], f.FromT, f.ToT] {
      override def hasOptional: Boolean = true
      override val default: Option[Option[T]] = f.default match {
        case Some(v) => Some(Some(v))
        case None    => None
      }

      // we must use Option instead of Some because
      // the underlying field may interpret custom values as null
      // eg. Unsafe enums are encoded as string and default "" is treated as None
      override def from(v: f.FromT)(cm: CaseMapper): Option[T] =
        if (v == null) None else Option(f.from(v)(cm))

      override def to(v: Option[T], b: Message.Builder)(cm: CaseMapper): f.ToT = v match {
        case None    => null.asInstanceOf[f.ToT]
        case Some(x) => f.to(x, b)(cm)
      }
    }

  implicit def pfIterable[T, C[_]](implicit
    f: ProtobufField[T],
    ti: C[T] => Iterable[T],
    fc: FactoryCompat[T, C[T]]
  ): ProtobufField[C[T]] =
    new Aux[C[T], ju.List[f.FromT], ju.List[f.ToT]] {
      override def hasOptional: Boolean = false
      override val default: Option[C[T]] = Some(fc.newBuilder.result())
      override def from(v: ju.List[f.FromT])(cm: CaseMapper): C[T] = {
        val b = fc.newBuilder
        if (v != null) {
          b ++= v.asScala.iterator.map(f.from(_)(cm))
        }
        b.result()
      }

      override def to(v: C[T], b: Message.Builder)(cm: CaseMapper): ju.List[f.ToT] =
        if (v.isEmpty) null else v.iterator.map(f.to(_, b)(cm)).toList.asJava
    }
}
