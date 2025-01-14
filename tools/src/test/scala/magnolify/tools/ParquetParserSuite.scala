/*
 * Copyright 2021 Spotify AB
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

package magnolify.tools

import magnolify.parquet._
import magnolify.test._

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetTime}
import scala.annotation.nowarn
import scala.reflect.ClassTag

@nowarn("cat=deprecation") // Suppress warnings from importing AvroCompat
class ParquetParserSuite extends MagnolifySuite {
  import ParquetParserSuite._

  private def test[T: ClassTag](schema: Record)(implicit t: ParquetType[T]): Unit =
    test[T](null, schema)

  private def test[T: ClassTag](suffix: String, schema: Record)(implicit
    t: ParquetType[T]
  ): Unit = {
    val name = className[T] + (if (suffix == null) "" else "." + suffix)
    test(name) {
      assertEquals(ParquetParser.parse(t.schema), schema)
    }
  }

  test[Primitives](
    Record(
      Some("Primitives"),
      None,
      List(
        Record.Field("b", None, Primitive.Boolean),
        Record.Field("i8", None, Primitive.Byte),
        Record.Field("i16", None, Primitive.Short),
        Record.Field("i32", None, Primitive.Int),
        Record.Field("i64", None, Primitive.Long),
        Record.Field("f", None, Primitive.Float),
        Record.Field("d", None, Primitive.Double),
        Record.Field("ba", None, Primitive.Bytes),
        Record.Field("s", None, Primitive.String),
        Record.Field("e", None, Primitive.String)
      )
    )
  )

  private val decimalSchema = Record(
    Some("Decimal"),
    None,
    List(Record.Field("bd", None, Primitive.BigDecimal))
  )

  {
    implicit val pfDecimal = ParquetField.decimal32(9)
    test[Decimal]("i32", decimalSchema)
  }

  {
    implicit val pfDecimal = ParquetField.decimal64(18)
    test[Decimal]("i64", decimalSchema)
  }

  {
    implicit val pfDecimal = ParquetField.decimalFixed(8, 18, 0)
    test[Decimal]("fixed", decimalSchema)
  }

  {
    implicit val pfDecimal = ParquetField.decimalBinary(20, 0)
    test[Decimal]("binary", decimalSchema)
  }

  test[Date](
    Record(Some("Date"), None, List(Record.Field("d", None, Primitive.LocalDate)))
  )

  private val dateTimeSchema = Record(
    Some("DateTime"),
    None,
    List(
      Record.Field("i", None, Primitive.Instant),
      Record.Field("dt", None, Primitive.LocalDateTime),
      Record.Field("ot", None, Primitive.OffsetTime),
      Record.Field("t", None, Primitive.LocalTime)
    )
  )

  {
    import magnolify.parquet.logical.millis._
    test[DateTime]("millis", dateTimeSchema)
  }

  {
    import magnolify.parquet.logical.micros._
    test[DateTime]("micros", dateTimeSchema)
  }

  {
    import magnolify.parquet.logical.nanos._
    test[DateTime]("nanos", dateTimeSchema)
  }

  private val compositeSchema = Record(
    Some("Composite"),
    None,
    List(
      Record.Field("o", None, Optional(Primitive.Int)),
      Record.Field("l", None, Repeated(Primitive.Int)),
      Record.Field("m", None, Mapped(Primitive.String, Primitive.Int))
    )
  )

  test[Composite](compositeSchema)

  {
    import magnolify.parquet.ParquetArray.AvroCompat._
    test[Composite]("Avro", compositeSchema)
  }

  private val innerSchema =
    Record(None, None, List(Record.Field("i", None, Primitive.Int)))
  private val outerSchema = Record(
    Some("Outer"),
    None,
    List(
      Record.Field("r", None, innerSchema),
      Record.Field("o", None, Optional(innerSchema)),
      Record.Field("l", None, Repeated(innerSchema)),
      Record.Field("m", None, Mapped(Primitive.String, innerSchema))
    )
  )

  test[Outer](outerSchema)
}

object ParquetParserSuite {
  object Color extends Enumeration {
    val Red, Green, Blue = Value
  }

  case class Primitives(
    b: Boolean,
    i8: Byte,
    i16: Short,
    i32: Int,
    i64: Long,
    f: Float,
    d: Double,
    ba: Array[Byte],
    s: String,
    e: Color.Value
  )

  case class Decimal(bd: BigDecimal)
  case class Date(d: LocalDate)
  case class DateTime(i: Instant, dt: LocalDateTime, ot: OffsetTime, t: LocalTime)
  case class Composite(o: Option[Int], l: List[Int], m: Map[String, Int])
  case class Inner(i: Int)
  case class Outer(r: Inner, o: Option[Inner], l: List[Inner], m: Map[String, Inner])
}
