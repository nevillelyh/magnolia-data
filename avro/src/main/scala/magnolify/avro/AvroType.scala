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

package magnolify.avro

import magnolia1._
import magnolify.shared.{doc => _, _}
import magnolify.shims.FactoryCompat
import org.apache.avro.generic.GenericData.EnumSymbol
import org.apache.avro.generic._
import org.apache.avro.{JsonProperties, LogicalType, LogicalTypes, Schema}
import org.joda.{time => joda}

import java.nio.{ByteBuffer, ByteOrder}
import java.time._
import java.{util => ju}
import scala.annotation.{implicitNotFound, nowarn}
import scala.collection.concurrent
import scala.collection.compat._
import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag

sealed trait AvroType[T] extends Converter[T, GenericRecord, GenericRecord] {
  val schema: Schema
  def apply(r: GenericRecord): T = from(r)
  def apply(t: T): GenericRecord = to(t)
}

object AvroType {
  implicit def apply[T: AvroField]: AvroType[T] = AvroType(CaseMapper.identity)

  def apply[T](cm: CaseMapper)(implicit f: AvroField[T]): AvroType[T] = {
    f match {
      case r: AvroField.Record[_] =>
        r.schema(cm) // fail fast on bad annotations
        new AvroType[T] {
          private val caseMapper: CaseMapper = cm
          @transient override lazy val schema: Schema = r.schema(caseMapper)
          override def from(v: GenericRecord): T = r.from(v)(caseMapper)
          override def to(v: T): GenericRecord = r.to(v)(caseMapper)
        }
      case _ =>
        throw new IllegalArgumentException(s"AvroType can only be created from Record. Got $f")
    }
  }
}

sealed trait AvroField[T] extends Serializable { self =>
  type FromT
  type ToT

  @transient private lazy val schemaCache: concurrent.Map[ju.UUID, Schema] =
    concurrent.TrieMap.empty

  protected def buildSchema(cm: CaseMapper): Schema
  def schema(cm: CaseMapper): Schema =
    schemaCache.getOrElseUpdate(cm.uuid, buildSchema(cm))

  // Convert default `T` to Avro schema default value
  def makeDefault(d: T)(cm: CaseMapper): Any = to(d)(cm)

  // Fallback Avro schema default value
  def fallbackDefault: Any = null

  def from(v: FromT)(cm: CaseMapper): T
  def to(v: T)(cm: CaseMapper): ToT

  def fromAny(v: Any)(cm: CaseMapper): T = from(v.asInstanceOf[FromT])(cm)
}

object AvroField {
  sealed trait Aux[T, From, To] extends AvroField[T] {
    override type FromT = From
    override type ToT = To
  }
  sealed trait Record[T] extends Aux[T, GenericRecord, GenericRecord]

  // ////////////////////////////////////////////////

  type Typeclass[T] = AvroField[T]

  def join[T](caseClass: CaseClass[Typeclass, T]): AvroField[T] = {
    if (caseClass.isValueClass) {
      val p = caseClass.parameters.head
      val tc = p.typeclass
      new AvroField[T] {
        override type FromT = tc.FromT
        override type ToT = tc.ToT
        override protected def buildSchema(cm: CaseMapper): Schema = tc.buildSchema(cm)
        override def from(v: FromT)(cm: CaseMapper): T = caseClass.construct(_ => tc.fromAny(v)(cm))
        override def to(v: T)(cm: CaseMapper): ToT = tc.to(p.dereference(v))(cm)
      }
    } else {
      new Record[T] {
        override protected def buildSchema(cm: CaseMapper): Schema = Schema
          .createRecord(
            caseClass.typeName.short,
            getDoc(caseClass.annotations, caseClass.typeName.full),
            caseClass.typeName.owner,
            false,
            caseClass.parameters.map { p =>
              new Schema.Field(
                cm.map(p.label),
                p.typeclass.schema(cm),
                getDoc(p.annotations, s"${caseClass.typeName.full}#${p.label}"),
                p.default
                  .map(d => p.typeclass.makeDefault(d)(cm))
                  .getOrElse(p.typeclass.fallbackDefault)
              )
            }.asJava
          )

        // `JacksonUtils.toJson` expects `Map[String, Any]` for `RECORD` defaults
        override def makeDefault(d: T)(cm: CaseMapper): ju.Map[String, Any] = {
          caseClass.parameters
            .map { p =>
              val name = cm.map(p.label)
              val value = p.typeclass.makeDefault(p.dereference(d))(cm)
              name -> value
            }
            .toMap
            .asJava
        }

        override def from(v: GenericRecord)(cm: CaseMapper): T =
          caseClass.construct { p =>
            p.typeclass.fromAny(v.get(p.index))(cm)
          }

        override def to(v: T)(cm: CaseMapper): GenericRecord =
          caseClass.parameters.foldLeft(new GenericData.Record(schema(cm))) { (r, p) =>
            r.put(p.index, p.typeclass.to(p.dereference(v))(cm))
            r
          }
      }
    }
  }

  private def getDoc(annotations: Seq[Any], name: String): String = {
    val docs = annotations.collect { case d: doc => d.toString }
    require(docs.size <= 1, s"More than one @doc annotation: $name")
    docs.headOption.orNull
  }

  @implicitNotFound("Cannot derive AvroField for sealed trait")
  private sealed trait Dispatchable[T]
  def split[T: Dispatchable](sealedTrait: SealedTrait[Typeclass, T]): AvroField[T] = ???

  implicit def gen[T]: AvroField[T] = macro Magnolia.gen[T]

  // ////////////////////////////////////////////////

  def apply[T](implicit f: AvroField[T]): AvroField[T] = f

  def from[T]: FromWord[T] = new FromWord[T]

  class FromWord[T] {
    def apply[U](f: T => U)(g: U => T)(implicit af: AvroField[T]): AvroField[U] =
      new Aux[U, af.FromT, af.ToT] {
        override protected def buildSchema(cm: CaseMapper): Schema = af.schema(cm)
        override def makeDefault(d: U)(cm: CaseMapper): Any = af.makeDefault(g(d))(cm)
        override def from(v: FromT)(cm: CaseMapper): U = f(af.from(v)(cm))
        override def to(v: U)(cm: CaseMapper): ToT = af.to(g(v))(cm)
      }
  }

  // ////////////////////////////////////////////////

  private def aux[T, From, To](tpe: Schema.Type)(f: From => T)(g: T => To): AvroField[T] =
    new Aux[T, From, To] {
      override protected def buildSchema(cm: CaseMapper): Schema = Schema.create(tpe)
      override def from(v: FromT)(cm: CaseMapper): T = f(v)
      override def to(v: T)(cm: CaseMapper): ToT = g(v)
    }

  private def aux2[T, Repr](tpe: Schema.Type)(f: Repr => T)(g: T => Repr): AvroField[T] =
    aux[T, Repr, Repr](tpe)(f)(g)

  private def id[T](tpe: Schema.Type): AvroField[T] = aux[T, T, T](tpe)(identity)(identity)

  implicit val afNull: AvroField[Null] = aux2[Null, Null](Schema.Type.NULL)(_ => null)(_ => null)
  implicit val afBoolean: AvroField[Boolean] = id[Boolean](Schema.Type.BOOLEAN)
  implicit val afInt: AvroField[Int] = id[Int](Schema.Type.INT)
  implicit val afLong: AvroField[Long] = id[Long](Schema.Type.LONG)
  implicit val afFloat: AvroField[Float] = id[Float](Schema.Type.FLOAT)
  implicit val afDouble: AvroField[Double] = id[Double](Schema.Type.DOUBLE)
  implicit val afByteBuffer: AvroField[ByteBuffer] = new Aux[ByteBuffer, ByteBuffer, ByteBuffer] {
    override protected def buildSchema(cm: CaseMapper): Schema = Schema.create(Schema.Type.BYTES)
    // `JacksonUtils.toJson` expects `Array[Byte]` for `BYTES` defaults
    override def makeDefault(d: ByteBuffer)(cm: CaseMapper): Array[Byte] = d.array()
    // copy to avoid issue in case original buffer is reused
    override def from(v: ByteBuffer)(cm: CaseMapper): ByteBuffer = {
      val ptr = v.asReadOnlyBuffer()
      ByteBuffer.allocate(ptr.remaining()).put(ptr)
    }
    override def to(v: ByteBuffer)(cm: CaseMapper): ByteBuffer = v
  }
  implicit val afBytes: AvroField[Array[Byte]] =
    AvroField.from[ByteBuffer](_.array())(ByteBuffer.wrap)

  implicit val afCharSequence: AvroField[CharSequence] = id[CharSequence](Schema.Type.STRING)
  implicit val afString: AvroField[String] = new Aux[String, String, String] {
    override protected def buildSchema(cm: CaseMapper): Schema = {
      val schema = Schema.create(Schema.Type.STRING)
      GenericData.setStringType(schema, GenericData.StringType.String)
      schema
    }
    override def from(v: String)(cm: CaseMapper): String = v
    override def to(v: String)(cm: CaseMapper): String = v
  }

  implicit def afEnum[T](implicit et: EnumType[T], lp: shapeless.LowPriority): AvroField[T] =
    // Avro 1.9+ added a type parameter for `GenericEnumSymbol`, breaking 1.8 compatibility
    // Some reader, i.e. `AvroParquetReader` reads enums as `Utf8`
    new Aux[T, AnyRef, EnumSymbol] {
      override protected def buildSchema(cm: CaseMapper): Schema = {
        val doc = getDoc(et.annotations, s"Enum ${et.namespace}.${et.name}")
        Schema.createEnum(et.name, doc, et.namespace, et.values.asJava)
      }
      // `JacksonUtils.toJson` expects `String` for `ENUM` defaults
      override def makeDefault(d: T)(cm: CaseMapper): String = et.to(d)
      override def from(v: FromT)(cm: CaseMapper): T = et.from(v.toString)
      override def to(v: T)(cm: CaseMapper): ToT = new GenericData.EnumSymbol(schema(cm), v)
    }

  implicit def afOption[T](implicit f: AvroField[T]): AvroField[Option[T]] =
    new Aux[Option[T], f.FromT, f.ToT] {
      override protected def buildSchema(cm: CaseMapper): Schema =
        Schema.createUnion(Schema.create(Schema.Type.NULL), f.schema(cm))
      // `Option[T]` is a `UNION` of `[NULL, T]` and must default to first type `NULL`
      override def makeDefault(d: Option[T])(cm: CaseMapper): JsonProperties.Null = {
        require(d.isEmpty, "Option[T] can only default to None")
        JsonProperties.NULL_VALUE
      }
      override def from(v: f.FromT)(cm: CaseMapper): Option[T] =
        if (v == null) None else Some(f.from(v)(cm))
      override def to(v: Option[T])(cm: CaseMapper): f.ToT = v match {
        case None    => null.asInstanceOf[f.ToT]
        case Some(x) => f.to(x)(cm)
      }
    }

  implicit def afEither[A, B](implicit
    fa: AvroField[A],
    fb: AvroField[B]
  ): AvroField[Either[A, B]] =
    new Aux[Either[A, B], Any, Any] {
      override protected def buildSchema(cm: CaseMapper): Schema =
        Schema.createUnion(fa.schema(cm), fb.schema(cm))
      // `Either[A, B]` is a `UNION` of `[A, B]` and must default to first type `A`
      override def makeDefault(d: Either[A, B])(cm: CaseMapper): fa.ToT = {
        require(d.isLeft, "Either[A, B] can only default to Left[A]")
        fa.to(d.left.get)(cm): @nowarn
      }
      override def from(v: Any)(cm: CaseMapper): Either[A, B] = {
        GenericData.get().resolveUnion(schema(cm), v) match {
          case 0 => Left(fa.from(v.asInstanceOf[fa.FromT])(cm))
          case 1 => Right(fb.from(v.asInstanceOf[fb.FromT])(cm))
        }
      }

      override def to(v: Either[A, B])(cm: CaseMapper): Any = v match {
        case Left(a)  => fa.to(a)(cm)
        case Right(b) => fb.to(b)(cm)
      }
    }

  implicit def afIterable[T, C[_]](implicit
    f: AvroField[T],
    ti: C[T] => Iterable[T],
    fc: FactoryCompat[T, C[T]]
  ): AvroField[C[T]] =
    new Aux[C[T], ju.List[f.FromT], GenericArray[f.ToT]] {
      override protected def buildSchema(cm: CaseMapper): Schema = Schema.createArray(f.schema(cm))
      override def fallbackDefault: ju.List[f.ToT] = ju.Collections.emptyList()
      override def from(v: ju.List[f.FromT])(cm: CaseMapper): C[T] =
        fc.fromSpecific(v.asScala.iterator.map(p => f.from(p)(cm)))
      override def to(v: C[T])(cm: CaseMapper): GenericArray[f.ToT] =
        new GenericData.Array[f.ToT](schema(cm), v.iterator.map(f.to(_)(cm)).toList.asJava)
    }

  implicit def afCharSequenceMap[T](implicit f: AvroField[T]): AvroField[Map[CharSequence, T]] =
    new Aux[Map[CharSequence, T], ju.Map[CharSequence, f.FromT], ju.Map[CharSequence, f.ToT]] {
      override protected def buildSchema(cm: CaseMapper): Schema = Schema.createMap(f.schema(cm))
      override def fallbackDefault: ju.Map[CharSequence, f.ToT] = ju.Collections.emptyMap()
      override def from(v: ju.Map[CharSequence, f.FromT])(cm: CaseMapper): Map[CharSequence, T] =
        v.asScala.map { case (k, v) => k -> f.from(v)(cm) }.toMap
      override def to(v: Map[CharSequence, T])(cm: CaseMapper): ju.Map[CharSequence, f.ToT] =
        v.map { case (k, v) => k -> f.to(v)(cm) }.asJava
    }

  implicit def afStringMap[T](implicit f: AvroField[T]): AvroField[Map[String, T]] =
    new Aux[Map[String, T], ju.Map[String, f.FromT], ju.Map[String, f.ToT]] {
      override protected def buildSchema(cm: CaseMapper): Schema = {
        val schema = Schema.createMap(f.schema(cm))
        GenericData.setStringType(schema, GenericData.StringType.String)
        schema
      }
      override def fallbackDefault: ju.Map[String, f.ToT] = ju.Collections.emptyMap()
      override def from(v: ju.Map[String, f.FromT])(cm: CaseMapper): Map[String, T] =
        v.asScala.map { case (k, v) => k -> f.from(v)(cm) }.toMap
      override def to(v: Map[String, T])(cm: CaseMapper): ju.Map[String, f.ToT] =
        v.map { case (k, v) => k -> f.to(v)(cm) }.asJava
    }
  def afMap[T: AvroField]: AvroField[Map[String, T]] = afStringMap

  // ////////////////////////////////////////////////

  def logicalType[T](lt: => LogicalType): LogicalTypeWord[T] = new LogicalTypeWord[T](lt)

  class LogicalTypeWord[T](lt: => LogicalType) extends Serializable {
    def apply[U](f: T => U)(g: U => T)(implicit af: AvroField[T]): AvroField[U] =
      new Aux[U, af.FromT, af.ToT] {
        override protected def buildSchema(cm: CaseMapper): Schema = {
          // `LogicalType#addToSchema` mutates `Schema`, make a copy first
          val schema = new Schema.Parser().parse(af.schema(cm).toString)
          lt.addToSchema(schema)
        }

        override def makeDefault(d: U)(cm: CaseMapper): Any = af.makeDefault(g(d))(cm)
        override def from(v: FromT)(cm: CaseMapper): U = f(af.from(v)(cm))
        override def to(v: U)(cm: CaseMapper): ToT = af.to(g(v))(cm)
      }
  }

  // https://avro.apache.org/docs/current/spec.html#Logical+Types
  // Precision and scale are not encoded in the `BigDecimal` type and must be specified
  def bigDecimal(precision: Int, scale: Int = 0): AvroField[BigDecimal] =
    logicalType[Array[Byte]](LogicalTypes.decimal(precision, scale))(
      Decimal.fromBytes(_, precision, scale)
    )(Decimal.toBytes(_, precision, scale))

  implicit val afUuid: AvroField[ju.UUID] =
    logicalType[CharSequence](LogicalTypes.uuid())(cs => ju.UUID.fromString(cs.toString))(
      _.toString
    )

  // date
  implicit val afDate: AvroField[LocalDate] =
    logicalType[Int](LogicalTypes.date())(x => LocalDate.ofEpochDay(x.toLong))(_.toEpochDay.toInt)
  private lazy val EpochJodaDate = new joda.LocalDate(1970, 1, 1)
  implicit val afJodaDate: AvroField[joda.LocalDate] =
    logicalType[Int](LogicalTypes.date()) { daysFromEpoch =>
      EpochJodaDate.plusDays(daysFromEpoch)
    } { date =>
      joda.Days.daysBetween(EpochJodaDate, date).getDays
    }

  // duration, as in the avro spec. do not make implicit as there is not a specific type for it
  // A duration logical type annotates Avro fixed type of size 12, which stores three little-endian unsigned integers
  // that represent durations at different granularities of time.
  // The first stores a number in months, the second stores a number in days, and the third stores a number in milliseconds.
  val afDuration: AvroField[(Long, Long, Long)] =
    logicalType[ByteBuffer](new LogicalType("duration")) { bs =>
      bs.order(ByteOrder.LITTLE_ENDIAN)
      val months = java.lang.Integer.toUnsignedLong(bs.getInt)
      val days = java.lang.Integer.toUnsignedLong(bs.getInt)
      val millis = java.lang.Integer.toUnsignedLong(bs.getInt)
      (months, days, millis)
    } { case (months, days, millis) =>
      ByteBuffer
        .allocate(12)
        .order(ByteOrder.LITTLE_ENDIAN)
        .putInt(months.toInt)
        .putInt(days.toInt)
        .putInt(millis.toInt)
    }(AvroField.fixed(12)(ByteBuffer.wrap)(_.array()))

  def fixed[T: ClassTag](
    size: Int
  )(f: Array[Byte] => T)(g: T => Array[Byte])(implicit an: AnnotationType[T]): AvroField[T] =
    new Aux[T, GenericFixed, GenericFixed] {
      override protected def buildSchema(cm: CaseMapper): Schema = {
        val n = ReflectionUtils.name[T]
        val ns = ReflectionUtils.namespace[T]
        Schema.createFixed(n, getDoc(an.annotations, n), ns, size)
      }

      override def from(v: GenericFixed)(cm: CaseMapper): T = f(v.bytes())
      override def to(v: T)(cm: CaseMapper): GenericFixed = new GenericData.Fixed(schema(cm), g(v))
    }
}
