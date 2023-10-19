/*
 * Copyright 2022 Spotify AB
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

package magnolify.neo4j

import magnolia1._
import magnolify.shared.{CaseMapper, Converter}
import magnolify.shims.FactoryCompat
import org.neo4j.driver.exceptions.value.ValueException
import org.neo4j.driver.types.{IsoDuration, Point}
import org.neo4j.driver.{Value, Values}

import java.time._
import scala.annotation.implicitNotFound
import scala.jdk.CollectionConverters._
import scala.collection.compat._

trait ValueType[T] extends Converter[T, Value, Value] {
  def apply(r: Value): T = from(r)
  def apply(t: T): Value = to(t)
}

object ValueType {

  implicit def apply[T: ValueField]: ValueType[T] = ValueType(CaseMapper.identity)

  def apply[T](cm: CaseMapper)(implicit f: ValueField[T]): ValueType[T] = f match {
    case r: ValueField.Record[_] =>
      new ValueType[T] {
        private val caseMapper: CaseMapper = cm
        override def from(v: Value): T = r.from(v)(caseMapper)
        override def to(v: T): Value = r.to(v)(caseMapper)
      }
    case _ =>
      throw new IllegalArgumentException(s"ValueType can only be created from Record. Got $f")
  }
}

sealed trait ValueField[T] extends Serializable {
  def from(v: Value)(cm: CaseMapper): T
  def to(v: T)(cm: CaseMapper): Value
}

object ValueField {
  sealed trait Record[T] extends ValueField[T]

  // ////////////////////////////////////////////////
  type Typeclass[T] = ValueField[T]

  def join[T](caseClass: CaseClass[Typeclass, T]): ValueField[T] = {
    if (caseClass.isValueClass) {
      val p = caseClass.parameters.head
      val tc = p.typeclass
      new ValueField[T] {
        override def from(v: Value)(cm: CaseMapper): T = caseClass.construct(_ => tc.from(v)(cm))
        override def to(v: T)(cm: CaseMapper): Value = tc.to(p.dereference(v))(cm)
      }
    } else {
      new Record[T] {
        override def from(v: Value)(cm: CaseMapper): T =
          caseClass.construct { p =>
            val field = cm.map(p.label)
            try {
              p.typeclass.from(v.get(field))(cm)
            } catch {
              case e: ValueException =>
                throw new RuntimeException(s"Failed to decode $field: ${e.getMessage}", e)
            }
          }

        override def to(v: T)(cm: CaseMapper): Value = {
          val jmap = caseClass.parameters
            .foldLeft(Map.newBuilder[String, AnyRef]) { (m, p) =>
              m += cm.map(p.label) -> p.typeclass.to(p.dereference(v))(cm)
              m
            }
            .result()
            .asJava
          Values.value(jmap)
        }
      }
    }
  }

  @implicitNotFound("Cannot derive AvroField for sealed trait")
  private sealed trait Dispatchable[T]

  def split[T: Dispatchable](sealedTrait: SealedTrait[Typeclass, T]): ValueField[T] = ???

  implicit def gen[T]: ValueField[T] = macro Magnolia.gen[T]

  // ////////////////////////////////////////////////

  def apply[T](implicit f: ValueField[T]): ValueField[T] = f

  def from[T]: FromWord[T] = new FromWord[T]

  class FromWord[T] {
    def apply[U](f: T => U)(g: U => T)(implicit af: ValueField[T]): ValueField[U] =
      new ValueField[U] {
        override def from(v: Value)(cm: CaseMapper): U = f(af.from(v)(cm))
        override def to(v: U)(cm: CaseMapper): Value = af.to(g(v))(cm)
      }
  }

  // ////////////////////////////////////////////////
  private def primitive[T](f: Value => T): ValueField[T] = new ValueField[T] {
    override def from(v: Value)(cm: CaseMapper): T = {
      // ensured that v isn't null otherwise neo4j silently coerces
      if (v.isNull) throw new ValueException("Cannot convert null value")
      f(v)
    }
    override def to(v: T)(cm: CaseMapper): Value = Values.value(v)
  }

  implicit val vfBoolean: ValueField[Boolean] = primitive(_.asBoolean())
  implicit val vfString: ValueField[String] = primitive(_.asString())
  implicit val vfChar: ValueField[Char] = primitive(_.asString().head)
  implicit val vfLong: ValueField[Long] = primitive(_.asLong())
  implicit val vfShort: ValueField[Short] = primitive(_.asInt().toShort)
  implicit val vfByte: ValueField[Byte] = primitive(_.asInt().toByte)
  implicit val vfInt: ValueField[Int] = primitive(_.asInt())
  implicit val vfDouble: ValueField[Double] = primitive(_.asDouble())
  implicit val vfFloat: ValueField[Float] = primitive(_.asFloat())
  implicit val vfLocalDate: ValueField[LocalDate] = primitive(_.asLocalDate())
  implicit val vfOffsetTime: ValueField[OffsetTime] = primitive(_.asOffsetTime())
  implicit val vfLocalTime: ValueField[LocalTime] = primitive(_.asLocalTime())
  implicit val vfLocalDateTime: ValueField[LocalDateTime] = primitive(_.asLocalDateTime())
  implicit val vfOffsetDateTime: ValueField[OffsetDateTime] = primitive(_.asOffsetDateTime())
  implicit val vfZonedDateTime: ValueField[ZonedDateTime] = primitive(_.asZonedDateTime())
  implicit val vfIsoDuration: ValueField[IsoDuration] = primitive(_.asIsoDuration())
  // Java Period & Duration are accepted but stored as IsoDuration
  // We need to convert them back when reading
  implicit val vfPeriod: ValueField[Period] = primitive { v =>
    val iso = v.asIsoDuration()
    val years = iso.months() / 12
    val months = iso.months() % 12
    Period.of(years.toInt, months.toInt, iso.days().toInt)
  }
  implicit val vfDuration: ValueField[Duration] = primitive { v =>
    val iso = v.asIsoDuration()
    Duration.ofSeconds(iso.seconds(), iso.nanoseconds().toLong)
  }
  implicit val vfPoint: ValueField[Point] = primitive(_.asPoint())

  implicit def vfOption[T](implicit f: ValueField[T]): ValueField[Option[T]] =
    new ValueField[Option[T]] {
      override def from(v: Value)(cm: CaseMapper): Option[T] = {
        v match {
          case Values.NULL => None
          case _           => Some(f.from(v)(cm))
        }
      }

      override def to(v: Option[T])(cm: CaseMapper): Value = v match {
        case None    => Values.NULL
        case Some(x) => f.to(x)(cm)
      }
    }

  implicit def vfIterable[T, C[_]](implicit
    f: ValueField[T],
    ti: C[T] => Iterable[T],
    fc: FactoryCompat[T, C[T]]
  ): ValueField[C[T]] = new ValueField[C[T]] {
    override def from(v: Value)(cm: CaseMapper): C[T] =
      fc.fromSpecific(v.asList[T]((v: Value) => f.from(v)(cm)).asScala)

    override def to(v: C[T])(cm: CaseMapper): Value =
      Values.value(v.iterator.map(f.to(_)(cm)).toList.asJava)
  }

  implicit def vfMap[T](implicit f: ValueField[T]): ValueField[Map[String, T]] =
    new ValueField[Map[String, T]] {
      override def from(v: Value)(cm: CaseMapper): Map[String, T] =
        v.asMap[T]((v: Value) => f.from(v)(cm)).asScala.toMap

      override def to(v: Map[String, T])(cm: CaseMapper): Value =
        Values.value(v.iterator.map(kv => (kv._1, f.to(kv._2)(cm))).toMap.asJava)
    }
}
