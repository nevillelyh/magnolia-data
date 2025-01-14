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

import java.time._
import cats._
import magnolify.cats.auto._
import magnolify.cats.TestEq._
import magnolify.avro.AvroType
import magnolify.shared.{doc, CaseMapper}
import magnolify.avro.unsafe._
import magnolify.parquet.unsafe._
import magnolify.parquet.util.AvroSchemaComparer
import magnolify.scalacheck.auto._
import magnolify.scalacheck.TestArbitrary._
import magnolify.test._
import magnolify.test.Simple._
import org.apache.avro.generic.GenericRecord
import org.apache.hadoop.conf.Configuration
import org.apache.parquet.avro.{
  AvroParquetReader,
  AvroParquetWriter,
  AvroReadSupport,
  AvroSchemaConverter,
  GenericDataSupplier
}
import org.scalacheck._

import scala.annotation.nowarn
import scala.reflect.ClassTag

@nowarn("cat=deprecation") // Suppress warnings from importing AvroCompat
class AvroParquetSuite extends MagnolifySuite {

  import magnolify.parquet.ParquetArray.AvroCompat._

  private def test[T: Arbitrary: ClassTag]()(implicit
    at: AvroType[T],
    tpe: ParquetType[T],
    eq: Eq[T]
  ): Unit = test[T](List.empty)

  private def test[T: Arbitrary: ClassTag](
    schemaErrors: List[String]
  )(implicit
    at: AvroType[T],
    tpe: ParquetType[T],
    eq: Eq[T]
  ): Unit = test[T](className[T], schemaErrors)

  private def test[T: Arbitrary](
    name: String,
    schemaErrors: List[String]
  )(implicit
    at: AvroType[T],
    tpe: ParquetType[T],
    eq: Eq[T]
  ): Unit = {
    // Ensure serializable even after evaluation of `schema` and `avroSchema`
    tpe.schema: Unit
    tpe.avroSchema: Unit
    val pt = ensureSerializable(tpe)

    property(s"$name.avro2parquet") {
      Prop.forAll { (t: T) =>
        val r = at(t)

        val out = new TestOutputFile
        val writer =
          AvroParquetWriter.builder[GenericRecord](out).withSchema(at.schema).build()
        writer.write(r)
        writer.close()

        val in = new TestInputFile(out.getBytes)
        val reader = pt.readBuilder(in).build()
        val copy = reader.read()
        val next = reader.read()
        reader.close()
        Prop.all(eq.eqv(t, copy), next == null)
      }
    }

    property(s"$name.parquet2avro") {
      Prop.forAll { (t: T) =>
        val out = new TestOutputFile
        val writer = pt.writeBuilder(out).build()
        writer.write(t)
        writer.close()

        val in = new TestInputFile(out.getBytes)
        val conf = new Configuration()
        AvroReadSupport.setAvroDataSupplier(conf, classOf[GenericDataSupplier])
        // read with AvroType schema instead of parquet writer one
        AvroReadSupport.setAvroReadSchema(conf, at.schema)
        val reader = AvroParquetReader.builder[GenericRecord](in).withConf(conf).build()

        val copy = reader.read()
        val next = reader.read()
        reader.close()
        Prop.all(eq.eqv(t, at(copy)), next == null)
      }
    }

    test(s"$name.schema") {
      val converter = new AvroSchemaConverter()
      val avro = converter.convert(at.schema)
      val parquet = pt.schema
      parquet.checkContains(avro)
      avro.checkContains(parquet)
    }

    test(s"$name.avroSchema") {
      val results = AvroSchemaComparer.compareSchemas(at.schema, pt.avroSchema)
      assertEquals(results, schemaErrors)
    }
  }

  test[Integers]()
  test[Floats]()
  test[Required]()
  test[Nullable]()

  test[Repeated]()
  test[Nested](
    List(
      "root.r 'name' are different 'Required' != 'r'",
      "root.r 'namespace' are different 'magnolify.test.Simple' != 'null'",
      "root.o 'name' are different 'Required' != 'o'",
      "root.o 'namespace' are different 'magnolify.test.Simple' != 'null'",
      "root.l 'name' are different 'Required' != 'array'",
      "root.l 'namespace' are different 'magnolify.test.Simple' != 'null'"
    )
  )

  test[Unsafe]()

  test[Collections]()
  test[MoreCollections]()

  test[Enums](
    List(
      "root.j 'name' are different 'Color' != 'string'",
      "root.j 'type' are different 'ENUM' != 'STRING'",
      "root.s 'name' are different 'Color' != 'string'",
      "root.s 'type' are different 'ENUM' != 'STRING'",
      "root.a 'name' are different 'Color' != 'string'",
      "root.a 'type' are different 'ENUM' != 'STRING'",
      "root.jo 'name' are different 'Color' != 'string'",
      "root.jo 'type' are different 'ENUM' != 'STRING'",
      "root.so 'name' are different 'Color' != 'string'",
      "root.so 'type' are different 'ENUM' != 'STRING'",
      "root.ao 'name' are different 'Color' != 'string'",
      "root.ao 'type' are different 'ENUM' != 'STRING'",
      "root.jr 'name' are different 'Color' != 'string'",
      "root.jr 'type' are different 'ENUM' != 'STRING'",
      "root.sr 'name' are different 'Color' != 'string'",
      "root.sr 'type' are different 'ENUM' != 'STRING'",
      "root.ar 'name' are different 'Color' != 'string'",
      "root.ar 'type' are different 'ENUM' != 'STRING'"
    )
  )
  test[UnsafeEnums]()

  test[ParquetTypes]()

  {
    import magnolify.avro.logical.bigquery._
    // Precision = number of digits, so 5 means -99999 to 99999
    val precision = 38
    val scale = 9
    val max = BigInt(10).pow(precision) - 1
    implicit val arbBigDecimal: Arbitrary[BigDecimal] =
      Arbitrary(Gen.choose(-max, max).map(BigDecimal(_, scale)))
    implicit val pfBigDecimal: ParquetField[BigDecimal] =
      ParquetField.decimalBinary(precision, scale)
    test[Decimal]()
  }

  test[AvroParquetLogical]()

  {
    import magnolify.avro.logical.millis._
    import magnolify.parquet.logical.millis._
    test[AvroParquetTimeMillis]()
  }

  {
    import magnolify.avro.logical.micros._
    import magnolify.parquet.logical.micros._
    test[AvroParquetTimeMicros]()
  }

  // nested record doc is lost
  test[AvroParquetWithNestedAnnotations](
    List(
      "root.nested 'name' are different 'AvroParquetWithAnnotations' != 'nested'",
      "root.nested 'doc' are different 'Should be ignored' != 'null'",
      "root.nested 'namespace' are different 'magnolify.parquet' != 'null'"
    )
  )

  {
    val upperCaseMapper = CaseMapper(_.toUpperCase())
    implicit val at = AvroType[AvroParquetWithNestedAnnotations](upperCaseMapper)
    implicit val pt = ParquetType[AvroParquetWithNestedAnnotations](upperCaseMapper)
    test[AvroParquetWithNestedAnnotations](
      "AvroParquetWithNestedAnnotations and CaseMapper",
      List(
        "root.NESTED 'name' are different 'AvroParquetWithAnnotations' != 'NESTED'",
        "root.NESTED 'doc' are different 'Should be ignored' != 'null'",
        "root.NESTED 'namespace' are different 'magnolify.parquet' != 'null'"
      )
    )
  }

  test[AvroParquetWithAnnotationsAndOptions](
    List(
      "root.nested 'name' are different 'AvroParquetWithAnnotations' != 'nested'",
      "root.nested 'doc' are different 'Should be ignored' != 'null'",
      "root.nested 'namespace' are different 'magnolify.parquet' != 'null'"
    )
  )

  test[AvroParquetWithAnnotationsAndLists](
    List(
      "root.nested 'name' are different 'AvroParquetWithAnnotations' != 'array'",
      "root.nested 'doc' are different 'Should be ignored' != 'null'",
      "root.nested 'namespace' are different 'magnolify.parquet' != 'null'"
    )
  )
}

case class AvroParquetLogical(d: LocalDate)
case class AvroParquetTimeMillis(i: Instant, dt: LocalDateTime, t: LocalTime)
case class AvroParquetTimeMicros(i: Instant, dt: LocalDateTime, t: LocalTime)
@doc("Should be ignored")
case class AvroParquetWithAnnotations(
  @doc("nested field policy") s: String,
  s2: String
)
@doc("root level policy")
case class AvroParquetWithNestedAnnotations(
  @doc("string policy") s: String,
  @doc("record policy") nested: AvroParquetWithAnnotations
)

@doc("root level policy")
case class AvroParquetWithAnnotationsAndOptions(
  @doc("string policy") s: String,
  @doc("record policy")
  nested: Option[AvroParquetWithAnnotations]
)

@doc("root level policy")
case class AvroParquetWithAnnotationsAndLists(
  @doc("string policy") s: String,
  @doc("record policy")
  nested: List[AvroParquetWithAnnotations]
)
