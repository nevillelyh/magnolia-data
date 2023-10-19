/*
 * Copyright 2019 Spotify AB
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

package magnolify.datastore

import java.time.Instant
import com.google.datastore.v1._
import com.google.datastore.v1.client.DatastoreHelper.makeValue
import com.google.protobuf.{ByteString, NullValue}
import magnolia1._
import magnolify.shared.{CaseMapper, Converter}
import magnolify.shims.FactoryCompat

import scala.annotation.{implicitNotFound, StaticAnnotation}
import scala.jdk.CollectionConverters._
import scala.collection.compat._

sealed trait EntityType[T] extends Converter[T, Entity, Entity.Builder] {
  def apply(v: Entity): T = from(v)
  def apply(v: T): Entity = to(v).build()
}

class key(val project: String = null, val namespace: String = null, val kind: String = null)
    extends StaticAnnotation
    with Serializable
class excludeFromIndexes(val exclude: Boolean = true) extends StaticAnnotation with Serializable

object EntityType {
  implicit def apply[T: EntityField]: EntityType[T] = EntityType(CaseMapper.identity)

  def apply[T](cm: CaseMapper)(implicit f: EntityField[T]): EntityType[T] = f match {
    case r: EntityField.Record[_] =>
      new EntityType[T] {
        private val caseMapper: CaseMapper = cm
        override def from(v: Entity): T = r.fromEntity(v)(caseMapper)
        override def to(v: T): Entity.Builder = r.toEntity(v)(caseMapper)
      }
    case _ =>
      throw new IllegalArgumentException(s"EntityType can only be created from Record. Got $f")
  }
}

sealed trait KeyField[T] extends Serializable { self =>
  def setKey(b: Key.PathElement.Builder, key: T): Key.PathElement.Builder

  def map[U](f: U => T): KeyField[U] = new KeyField[U] {
    override def setKey(b: Key.PathElement.Builder, key: U): Key.PathElement.Builder =
      self.setKey(b, f(key))
  }
}

object KeyField {
  implicit val longKeyField: KeyField[Long] = new KeyField[Long] {
    override def setKey(b: Key.PathElement.Builder, key: Long): Key.PathElement.Builder =
      b.setId(key)
  }
  implicit val stringKeyField: KeyField[String] = new KeyField[String] {
    override def setKey(b: Key.PathElement.Builder, key: String): Key.PathElement.Builder =
      b.setName(key)
  }

  def at[T]: FromWord[T] = new FromWord[T]

  class FromWord[T] {
    def apply[U](f: T => U)(implicit kf: KeyField[U]): KeyField[T] = kf.map(f)
  }

  class NotSupported[T] extends KeyField[T] {
    override def setKey(b: Key.PathElement.Builder, key: T): Key.PathElement.Builder = ???
    override def map[U](f: U => T): KeyField[U] = new NotSupported[U]
  }

  implicit def notSupportedKeyField[T]: KeyField[T] = new NotSupported[T]
}

sealed trait EntityField[T] extends Serializable {
  def keyField: KeyField[T]
  def from(v: Value)(cm: CaseMapper): T
  def to(v: T)(cm: CaseMapper): Value.Builder
}

object EntityField {

  sealed trait Record[T] extends EntityField[T] {
    def fromEntity(v: Entity)(cm: CaseMapper): T
    def toEntity(v: T)(cm: CaseMapper): Entity.Builder

    override def from(v: Value)(cm: CaseMapper): T = fromEntity(v.getEntityValue)(cm)
    override def to(v: T)(cm: CaseMapper): Value.Builder =
      Value.newBuilder().setEntityValue(toEntity(v)(cm))
  }

  // ////////////////////////////////////////////////

  type Typeclass[T] = EntityField[T]

  def join[T: KeyField](caseClass: CaseClass[Typeclass, T]): EntityField[T] = {
    if (caseClass.isValueClass) {
      val p = caseClass.parameters.head
      val tc = p.typeclass
      new EntityField[T] {
        override lazy val keyField: KeyField[T] = tc.keyField.map(p.dereference)
        override def from(v: Value)(cm: CaseMapper): T = caseClass.construct(_ => tc.from(v)(cm))
        override def to(v: T)(cm: CaseMapper): Value.Builder = tc.to(p.dereference(v))(cm)
      }
    } else {
      new Record[T] {
        private val (keyIndex, keyOpt): (Int, Option[key]) = {
          val keys = caseClass.parameters
            .map(p => p -> getKey(p.annotations, s"${caseClass.typeName.full}#${p.label}"))
            .filter(_._2.isDefined)
          require(
            keys.size <= 1,
            s"More than one field with @key annotation: ${caseClass.typeName.full}#[${keys.map(_._1.label).mkString(", ")}]"
          )
          keys.headOption match {
            case None => (-1, None)
            case Some((p, k)) =>
              require(
                !p.typeclass.keyField.isInstanceOf[KeyField.NotSupported[_]],
                s"No KeyField[T] instance: ${caseClass.typeName.full}#${p.label}"
              )
              (p.index, k)
          }
        }

        private val excludeFromIndexes: Array[Boolean] = {
          val a = new Array[Boolean](caseClass.parameters.length)
          caseClass.parameters.foreach { p =>
            a(p.index) =
              getExcludeFromIndexes(p.annotations, s"${caseClass.typeName.full}#${p.label}")
          }
          a
        }

        override val keyField: KeyField[T] = implicitly[KeyField[T]]

        override def fromEntity(v: Entity)(cm: CaseMapper): T =
          caseClass.construct { p =>
            val f = v.getPropertiesOrDefault(cm.map(p.label), null)
            if (f == null && p.default.isDefined) {
              p.default.get
            } else {
              p.typeclass.from(f)(cm)
            }
          }

        override def toEntity(v: T)(cm: CaseMapper): Entity.Builder =
          caseClass.parameters.foldLeft(Entity.newBuilder()) { (eb, p) =>
            val value = p.dereference(v)
            val vb = p.typeclass.to(value)(cm)
            if (vb != null) {
              eb.putProperties(
                cm.map(p.label),
                vb.setExcludeFromIndexes(excludeFromIndexes(p.index))
                  .build()
              )
            }
            if (p.index == keyIndex) {
              val k = keyOpt.get
              val partitionId = {
                val b = PartitionId.newBuilder()
                if (k.project != null) {
                  b.setProjectId(k.project)
                }
                b.setNamespaceId(if (k.namespace != null) k.namespace else caseClass.typeName.owner)
              }
              val path = {
                val b = Key.PathElement.newBuilder()
                b.setKind(if (k.kind != null) k.kind else caseClass.typeName.short)
                p.typeclass.keyField.setKey(b, value)
              }
              val kb = Key
                .newBuilder()
                .setPartitionId(partitionId)
                .addPath(path)
              eb.setKey(kb)
            }
            eb
          }

        private def getKey(annotations: Seq[Any], name: String): Option[key] = {
          val keys = annotations.collect { case k: key => k }
          require(keys.size <= 1, s"More than one @key annotation: $name")
          keys.headOption
        }

        private def getExcludeFromIndexes(annotations: Seq[Any], name: String): Boolean = {
          val excludes = annotations.collect { case e: excludeFromIndexes => e.exclude }
          require(excludes.size <= 1, s"More than one @excludeFromIndexes annotation: $name")
          excludes.headOption.getOrElse(false)
        }
      }
    }
  }

  @implicitNotFound("Cannot derive EntityField for sealed trait")
  private sealed trait Dispatchable[T]
  def split[T: Dispatchable](sealedTrait: SealedTrait[Typeclass, T]): EntityField[T] = ???

  implicit def gen[T]: EntityField[T] = macro Magnolia.gen[T]

  // ////////////////////////////////////////////////

  def apply[T](implicit f: EntityField[T]): EntityField[T] = f

  def at[T](f: Value => T)(g: T => Value.Builder)(implicit kf: KeyField[T]): EntityField[T] =
    new EntityField[T] {
      override val keyField: KeyField[T] = kf
      override def from(v: Value)(cm: CaseMapper): T = f(v)
      override def to(v: T)(cm: CaseMapper): Value.Builder = g(v)
    }

  def from[T]: FromWord[T] = new FromWord[T]

  class FromWord[T] {
    def apply[U](f: T => U)(g: U => T)(implicit ef: EntityField[T]): EntityField[U] =
      new EntityField[U] {
        override val keyField: KeyField[U] = ef.keyField.map(g)
        override def from(v: Value)(cm: CaseMapper): U = f(ef.from(v)(cm))
        override def to(v: U)(cm: CaseMapper): Value.Builder = ef.to(g(v))(cm)
      }
  }

  // ////////////////////////////////////////////////

  // Entity key supports `Long` and `String` natively
  implicit val efLong: EntityField[Long] = at[Long](_.getIntegerValue)(makeValue)
  implicit val efString: EntityField[String] = at[String](_.getStringValue)(makeValue)

  // `Boolean`, `Double` and `Unit` should not be used as keys
  implicit def efBool(implicit kf: KeyField[Boolean]): EntityField[Boolean] =
    at[Boolean](_.getBooleanValue)(makeValue)
  implicit def efDouble(implicit kf: KeyField[Double]): EntityField[Double] =
    at[Double](_.getDoubleValue)(makeValue)
  implicit def efUnit(implicit kf: KeyField[Unit]): EntityField[Unit] =
    at[Unit](_ => ())(_ => Value.newBuilder().setNullValue(NullValue.NULL_VALUE))

  // User must provide `KeyField[T]` instances for `ByteString` and `Array[Byte]`
  implicit def efByteString(implicit kf: KeyField[ByteString]): EntityField[ByteString] =
    at[ByteString](_.getBlobValue)(makeValue)
  implicit def efByteArray(implicit kf: KeyField[Array[Byte]]): EntityField[Array[Byte]] =
    at[Array[Byte]](_.getBlobValue.toByteArray)(v => makeValue(ByteString.copyFrom(v)))

  // Encode `Instant` key as `Long`
  implicit val efTimestamp: EntityField[Instant] = {
    implicit val kfInstant = KeyField.at[Instant](_.toEpochMilli)
    at(TimestampConverter.toInstant)(TimestampConverter.fromInstant)
  }

  implicit def efOption[T](implicit f: EntityField[T]): EntityField[Option[T]] =
    new EntityField[Option[T]] {
      override val keyField: KeyField[Option[T]] = new KeyField[Option[T]] {
        override def setKey(b: Key.PathElement.Builder, key: Option[T]): Key.PathElement.Builder =
          key match {
            case None    => b
            case Some(k) => f.keyField.setKey(b, k)
          }
      }

      override def from(v: Value)(cm: CaseMapper): Option[T] =
        if (v == null) None else Some(f.from(v)(cm))
      override def to(v: Option[T])(cm: CaseMapper): Value.Builder = v match {
        case None    => null
        case Some(x) => f.to(x)(cm)
      }
    }

  implicit def efIterable[T, C[_]](implicit
    f: EntityField[T],
    kf: KeyField[C[T]],
    ti: C[T] => Iterable[T],
    fc: FactoryCompat[T, C[T]]
  ): EntityField[C[T]] =
    new EntityField[C[T]] {
      override val keyField: KeyField[C[T]] = kf
      override def from(v: Value)(cm: CaseMapper): C[T] = {
        val b = fc.newBuilder
        if (v != null) {
          b ++= v.getArrayValue.getValuesList.asScala.iterator.map(f.from(_)(cm))
        }
        b.result()
      }

      override def to(v: C[T])(cm: CaseMapper): Value.Builder =
        if (v.isEmpty) {
          null
        } else {
          Value
            .newBuilder()
            .setArrayValue(
              v.foldLeft(ArrayValue.newBuilder())((b, x) => b.addValues(f.to(x)(cm)))
                .build()
            )
        }
    }
}
