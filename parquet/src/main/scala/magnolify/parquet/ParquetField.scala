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

package magnolify.parquet

import magnolia1._
import magnolify.shared.{Converter => _, _}
import magnolify.shims.FactoryCompat

import java.nio.{ByteBuffer, ByteOrder}
import java.time.LocalDate
import java.util.UUID
import org.apache.parquet.io.ParquetDecodingException
import org.apache.parquet.io.api._
import org.apache.parquet.schema.PrimitiveType.PrimitiveTypeName
import org.apache.parquet.schema.Type.Repetition
import org.apache.parquet.schema.{LogicalTypeAnnotation, Type, Types}

import scala.annotation.implicitNotFound
import scala.collection.concurrent
import scala.collection.compat._

sealed trait ParquetField[T] extends Serializable {

  @transient private lazy val schemaCache: concurrent.Map[UUID, Type] =
    concurrent.TrieMap.empty

  protected def buildSchema(cm: CaseMapper): Type
  def schema(cm: CaseMapper): Type =
    schemaCache.getOrElseUpdate(cm.uuid, buildSchema(cm))

  def hasAvroArray: Boolean = false
  def fieldDocs(cm: CaseMapper): Map[String, String]
  def typeDoc: Option[String]

  protected def isGroup: Boolean = false
  protected def isEmpty(v: T): Boolean
  def write(c: RecordConsumer, v: T)(cm: CaseMapper): Unit
  def newConverter(): TypeConverter[T]

  protected def writeGroup(c: RecordConsumer, v: T)(cm: CaseMapper): Unit = {
    if (isGroup) {
      c.startGroup()
    }
    write(c, v)(cm)
    if (isGroup) {
      c.endGroup()
    }
  }
}

object ParquetField {
  sealed trait Record[T] extends ParquetField[T] {
    override protected def isGroup: Boolean = true
    override protected def isEmpty(v: T): Boolean = false
  }

  // ////////////////////////////////////////////
  type Typeclass[T] = ParquetField[T]

  def join[T](caseClass: CaseClass[Typeclass, T]): ParquetField[T] = {
    if (caseClass.isValueClass) {
      val p = caseClass.parameters.head
      val tc = p.typeclass
      new ParquetField[T] {
        override protected def buildSchema(cm: CaseMapper): Type = tc.buildSchema(cm)
        override protected def isEmpty(v: T): Boolean = tc.isEmpty(p.dereference(v))
        override def write(c: RecordConsumer, v: T)(cm: CaseMapper): Unit =
          tc.writeGroup(c, p.dereference(v))(cm)
        override def newConverter(): TypeConverter[T] = {
          val buffered = tc.newConverter()
            .asInstanceOf[TypeConverter.Buffered[p.PType]]
          new TypeConverter.Delegate[p.PType, T](buffered) {
            override def get: T = inner.get(b => caseClass.construct(_ => b.head))
          }
        }
        override def fieldDocs(cm: CaseMapper): Map[String, String] = Map.empty
        override def typeDoc: Option[String] = None
      }
    } else {
      new Record[T] {
        override def buildSchema(cm: CaseMapper): Type =
          caseClass.parameters
            .foldLeft(Types.requiredGroup()) { (g, p) =>
              g.addField(Schema.rename(p.typeclass.schema(cm), cm.map(p.label)))
            }
            .named(caseClass.typeName.full)

        override val hasAvroArray: Boolean = caseClass.parameters.exists(_.typeclass.hasAvroArray)

        override def fieldDocs(cm: CaseMapper): Map[String, String] =
          caseClass.parameters.flatMap { param =>
            val label = cm.map(param.label)
            val nestedDocs = param.typeclass.fieldDocs(cm).map { case (k, v) =>
              s"$label.$k" -> v
            }

            val collectedAnnValue = getDoc(
              param.annotations,
              s"Field ${caseClass.typeName}.$label"
            )
            val joinedAnnotations = collectedAnnValue match {
              case Some(value) => nestedDocs + (label -> value)
              case None        => nestedDocs
            }
            joinedAnnotations
          }.toMap

        override val typeDoc: Option[String] = getDoc(
          caseClass.annotations,
          s"Type ${caseClass.typeName}"
        )

        override def write(c: RecordConsumer, v: T)(cm: CaseMapper): Unit = {
          caseClass.parameters.foreach { p =>
            val x = p.dereference(v)
            if (!p.typeclass.isEmpty(x)) {
              val name = cm.map(p.label)
              c.startField(name, p.index)
              p.typeclass.writeGroup(c, x)(cm)
              c.endField(name, p.index)
            }
          }
        }

        override def newConverter(): TypeConverter[T] =
          new GroupConverter with TypeConverter.Buffered[T] {
            private val fieldConverters = caseClass.parameters.map(_.typeclass.newConverter())

            override def isPrimitive: Boolean = false

            override def getConverter(fieldIndex: Int): Converter = fieldConverters(fieldIndex)

            override def start(): Unit = ()

            override def end(): Unit = {
              val value = caseClass.construct { p =>
                try {
                  fieldConverters(p.index).get
                } catch {
                  case e: IllegalArgumentException =>
                    val field = s"${caseClass.typeName.full}#${p.label}"
                    throw new ParquetDecodingException(
                      s"Failed to decode $field: ${e.getMessage}",
                      e
                    )
                }
              }
              addValue(value)
            }
          }
      }
    }

  }

  @implicitNotFound("Cannot derive ParquetType for sealed trait")
  private sealed trait Dispatchable[T]
  def split[T: Dispatchable](sealedTrait: SealedTrait[Typeclass, T]): ParquetField[T] = ???

  implicit def apply[T]: ParquetField[T] = macro Magnolia.gen[T]

  private def getDoc(annotations: Seq[Any], name: String): Option[String] = {
    val docs = annotations.collect { case d: magnolify.shared.doc => d.toString }
    require(docs.size <= 1, s"More than one @doc annotation: $name")
    docs.headOption
  }

  // ////////////////////////////////////////////////

  def from[T]: FromWord[T] = new FromWord[T]

  class FromWord[T] {
    def apply[U](f: T => U)(g: U => T)(implicit pf: Primitive[T]): Primitive[U] =
      new Primitive[U] {
        override def buildSchema(cm: CaseMapper): Type = pf.schema(cm)
        override def write(c: RecordConsumer, v: U)(cm: CaseMapper): Unit = pf.write(c, g(v))(cm)
        override def newConverter(): TypeConverter[U] =
          pf.newConverter().asInstanceOf[TypeConverter.Primitive[T]].map(f)
        override type ParquetT = pf.ParquetT
      }
  }

  // ////////////////////////////////////////////////

  sealed trait Primitive[T] extends ParquetField[T] {
    override protected def isEmpty(v: T): Boolean = false
    override def fieldDocs(cm: CaseMapper): Map[String, String] = Map.empty
    override def typeDoc: Option[String] = None
    type ParquetT <: Comparable[ParquetT]
  }

  def primitive[T, UnderlyingT <: Comparable[UnderlyingT]](
    f: RecordConsumer => T => Unit,
    g: => TypeConverter[T],
    ptn: PrimitiveTypeName,
    lta: => LogicalTypeAnnotation = null
  ): Primitive[T] =
    new Primitive[T] {
      override def buildSchema(cm: CaseMapper): Type = Schema.primitive(ptn, lta)
      override def write(c: RecordConsumer, v: T)(cm: CaseMapper): Unit = f(c)(v)
      override def newConverter(): TypeConverter[T] = g
      override type ParquetT = UnderlyingT
    }

  implicit val pfBoolean: Primitive[Boolean] =
    primitive[Boolean, java.lang.Boolean](
      _.addBoolean,
      TypeConverter.newBoolean,
      PrimitiveTypeName.BOOLEAN
    )

  implicit val pfByte: Primitive[Byte] =
    primitive[Byte, Integer](
      c => v => c.addInteger(v.toInt),
      TypeConverter.newInt.map(_.toByte),
      PrimitiveTypeName.INT32,
      LogicalTypeAnnotation.intType(8, true)
    )
  implicit val pfShort: Primitive[Short] =
    primitive[Short, Integer](
      c => v => c.addInteger(v.toInt),
      TypeConverter.newInt.map(_.toShort),
      PrimitiveTypeName.INT32,
      LogicalTypeAnnotation.intType(16, true)
    )
  implicit val pfInt: Primitive[Int] =
    primitive[Int, Integer](
      _.addInteger,
      TypeConverter.newInt,
      PrimitiveTypeName.INT32,
      LogicalTypeAnnotation.intType(32, true)
    )
  implicit val pfLong: Primitive[Long] =
    primitive[Long, java.lang.Long](
      _.addLong,
      TypeConverter.newLong,
      PrimitiveTypeName.INT64,
      LogicalTypeAnnotation.intType(64, true)
    )
  implicit val pfFloat: Primitive[Float] =
    primitive[Float, java.lang.Float](_.addFloat, TypeConverter.newFloat, PrimitiveTypeName.FLOAT)

  implicit val pfDouble: Primitive[Double] =
    primitive[Double, java.lang.Double](
      _.addDouble,
      TypeConverter.newDouble,
      PrimitiveTypeName.DOUBLE
    )

  implicit val pfByteArray: Primitive[Array[Byte]] =
    primitive[Array[Byte], Binary](
      c => v => c.addBinary(Binary.fromConstantByteArray(v)),
      TypeConverter.newByteArray,
      PrimitiveTypeName.BINARY
    )
  implicit val pfString: Primitive[String] =
    primitive[String, Binary](
      c => v => c.addBinary(Binary.fromString(v)),
      TypeConverter.newString,
      PrimitiveTypeName.BINARY,
      LogicalTypeAnnotation.stringType()
    )

  implicit def pfOption[T](implicit t: ParquetField[T]): ParquetField[Option[T]] =
    new ParquetField[Option[T]] {
      override def buildSchema(cm: CaseMapper): Type =
        Schema.setRepetition(t.schema(cm), Repetition.OPTIONAL)
      override protected def isEmpty(v: Option[T]): Boolean = v.isEmpty

      override def fieldDocs(cm: CaseMapper): Map[String, String] = t.fieldDocs(cm)

      override def typeDoc: Option[String] = None

      override def write(c: RecordConsumer, v: Option[T])(cm: CaseMapper): Unit =
        v.foreach(t.writeGroup(c, _)(cm))

      override def newConverter(): TypeConverter[Option[T]] = {
        val buffered = t.newConverter()
          .asInstanceOf[TypeConverter.Buffered[T]]
          .withRepetition(Repetition.OPTIONAL)
        new TypeConverter.Delegate[T, Option[T]](buffered) {
          override def get: Option[T] = inner.get(_.headOption)
        }
      }
    }

  private val AvroArrayField = "array"
  implicit def ptIterable[T, C[T]](implicit
    t: ParquetField[T],
    ti: C[T] => Iterable[T],
    fc: FactoryCompat[T, C[T]],
    pa: ParquetArray
  ): ParquetField[C[T]] = {
    new ParquetField[C[T]] {
      override val hasAvroArray: Boolean = pa match {
        case ParquetArray.default               => false
        case ParquetArray.AvroCompat.avroCompat => true
      }

      override def buildSchema(cm: CaseMapper): Type = {
        val repeatedSchema = Schema.setRepetition(t.schema(cm), Repetition.REPEATED)
        if (hasAvroArray) {
          Types
            .requiredGroup()
            .addField(Schema.rename(repeatedSchema, AvroArrayField))
            .as(LogicalTypeAnnotation.listType())
            .named(t.schema(cm).getName)
        } else {
          repeatedSchema
        }
      }

      override protected def isGroup: Boolean = hasAvroArray
      override protected def isEmpty(v: C[T]): Boolean = v.isEmpty

      override def write(c: RecordConsumer, v: C[T])(cm: CaseMapper): Unit =
        if (hasAvroArray) {
          c.startField(AvroArrayField, 0)
          v.foreach(t.writeGroup(c, _)(cm))
          c.endField(AvroArrayField, 0)
        } else {
          v.foreach(t.writeGroup(c, _)(cm))
        }

      override def newConverter(): TypeConverter[C[T]] = {
        val buffered = t.newConverter()
          .asInstanceOf[TypeConverter.Buffered[T]]
          .withRepetition(Repetition.REPEATED)
        val arrayConverter = new TypeConverter.Delegate[T, C[T]](buffered) {
          override def get: C[T] = inner.get(fc.fromSpecific)
        }

        if (hasAvroArray) {
          new GroupConverter with TypeConverter.Buffered[C[T]] {
            override def getConverter(fieldIndex: Int): Converter = {
              require(fieldIndex == 0, "Avro array field index != 0")
              arrayConverter
            }
            override def start(): Unit = ()
            override def end(): Unit = addValue(arrayConverter.get)
            override def get: C[T] = get(_.headOption.getOrElse(fc.newBuilder.result()))
          }
        } else {
          arrayConverter
        }
      }

      override def fieldDocs(cm: CaseMapper): Map[String, String] = t.fieldDocs(cm)

      override def typeDoc: Option[String] = None
    }
  }

  // ////////////////////////////////////////////////

  def logicalType[T](lta: => LogicalTypeAnnotation): LogicalTypeWord[T] =
    new LogicalTypeWord[T](lta)

  class LogicalTypeWord[T](lta: => LogicalTypeAnnotation) extends Serializable {
    def apply[U](f: T => U)(g: U => T)(implicit pf: Primitive[T]): Primitive[U] = new Primitive[U] {
      override def buildSchema(cm: CaseMapper): Type = Schema.setLogicalType(pf.schema(cm), lta)
      override def write(c: RecordConsumer, v: U)(cm: CaseMapper): Unit = pf.write(c, g(v))(cm)
      override def newConverter(): TypeConverter[U] =
        pf.newConverter().asInstanceOf[TypeConverter.Primitive[T]].map(f)

      override type ParquetT = pf.ParquetT
    }
  }

  // https://github.com/apache/parquet-format/blob/master/LogicalTypes.md
  // Precision and scale are not encoded in the `BigDecimal` type and must be specified
  def decimal32(precision: Int, scale: Int = 0): Primitive[BigDecimal] = {
    require(1 <= precision && precision <= 9, s"Precision for INT32 $precision not within [1, 9]")
    require(0 <= scale && scale < precision, s"Scale $scale not within [0, $precision)")
    logicalType[Int](LogicalTypeAnnotation.decimalType(scale, precision))(x =>
      BigDecimal(BigInt(x), scale)
    )(_.underlying().unscaledValue().intValue())
  }

  def decimal64(precision: Int, scale: Int = 0): Primitive[BigDecimal] = {
    require(1 <= precision && precision <= 18, s"Precision for INT64 $precision not within [1, 18]")
    require(0 <= scale && scale < precision, s"Scale $scale not within [0, $precision)")
    logicalType[Long](LogicalTypeAnnotation.decimalType(scale, precision))(x =>
      BigDecimal(BigInt(x), scale)
    )(_.underlying().unscaledValue().longValue())
  }

  def decimalFixed(length: Int, precision: Int, scale: Int = 0): Primitive[BigDecimal] = {
    val capacity = math.floor(math.log10(math.pow(2, (8 * length - 1).toDouble) - 1)).toInt
    require(
      1 <= precision && precision <= capacity,
      s"Precision for FIXED($length) not within [1, $capacity]"
    )

    new Primitive[BigDecimal] {
      override def buildSchema(cm: CaseMapper): Type =
        Schema.primitive(
          PrimitiveTypeName.FIXED_LEN_BYTE_ARRAY,
          LogicalTypeAnnotation.decimalType(scale, precision),
          length
        )

      override def write(c: RecordConsumer, v: BigDecimal)(cm: CaseMapper): Unit =
        c.addBinary(Binary.fromConstantByteArray(Decimal.toFixed(v, precision, scale, length)))

      override def newConverter(): TypeConverter[BigDecimal] = TypeConverter.newByteArray.map { ba =>
        Decimal.fromBytes(ba, precision, scale)
      }

      override type ParquetT = Binary
    }
  }

  def decimalBinary(precision: Int, scale: Int = 0): Primitive[BigDecimal] = {
    require(1 <= precision, s"Precision $precision <= 0")
    require(0 <= scale && scale < precision, s"Scale $scale not within [0, $precision)")
    logicalType[Array[Byte]](LogicalTypeAnnotation.decimalType(scale, precision))(
      Decimal.fromBytes(_, precision, scale)
    )(Decimal.toBytes(_, precision, scale))
  }

  implicit def pfEnum[T](implicit et: EnumType[T]): Primitive[T] =
    logicalType[String](LogicalTypeAnnotation.enumType())(et.from)(et.to)

  implicit val ptUuid: Primitive[UUID] = new Primitive[UUID] {
    override def buildSchema(cm: CaseMapper): Type =
      Schema.primitive(PrimitiveTypeName.FIXED_LEN_BYTE_ARRAY, length = 16)

    override def write(c: RecordConsumer, v: UUID)(cm: CaseMapper): Unit =
      c.addBinary(
        Binary.fromConstantByteArray(
          ByteBuffer
            .allocate(16)
            .order(ByteOrder.BIG_ENDIAN)
            .putLong(v.getMostSignificantBits)
            .putLong(v.getLeastSignificantBits)
            .array()
        )
      )

    override def newConverter(): TypeConverter[UUID] = TypeConverter.newByteArray.map { ba =>
      val bb = ByteBuffer.wrap(ba)
      val h = bb.getLong
      val l = bb.getLong
      new UUID(h, l)
    }

    override type ParquetT = Binary
  }

  implicit val ptDate: Primitive[LocalDate] =
    logicalType[Int](LogicalTypeAnnotation.dateType())(x => LocalDate.ofEpochDay(x.toLong))(
      _.toEpochDay.toInt
    )

}
