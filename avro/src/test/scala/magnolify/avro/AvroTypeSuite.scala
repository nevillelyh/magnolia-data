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

import cats._
import com.fasterxml.jackson.core.JsonFactory
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import magnolify.avro.unsafe._
import magnolify.cats.TestEq._
import magnolify.cats.auto._
import magnolify.scalacheck.TestArbitrary._
import magnolify.scalacheck.auto._
import magnolify.shared.CaseMapper
import magnolify.shared.TestEnumType._
import magnolify.test.Simple._
import magnolify.test._
import org.apache.avro.Schema
import org.apache.avro.generic._
import org.apache.avro.io.{DecoderFactory, EncoderFactory}
import org.apache.avro.util.Utf8
import org.joda.{time => joda}
import org.scalacheck._

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.net.URI
import java.nio.ByteBuffer
import java.time._
import java.time.format.DateTimeFormatter
import java.util
import java.util.{Objects, UUID}
import scala.jdk.CollectionConverters._
import scala.reflect._
import scala.util.Try

class AvroTypeSuite extends MagnolifySuite {

  // Best effort for GenericRecord eq
  // will fail if model contains List[CharSequence] / Map[_, CharSequence] / Option[CharSequence]
  val eqr: Eq[GenericRecord] = new Eq[GenericRecord] {
    private def read[T](f: Schema.Field)(r: GenericRecord): T = r.get(f.pos()).asInstanceOf[T]

    override def eqv(x: GenericRecord, y: GenericRecord): Boolean = {
      x.getSchema == y.getSchema && x.getSchema.getFields.asScala.forall { f =>
        val fs = f.schema()
        fs.getType match {
          case Schema.Type.ARRAY if fs.getElementType.getType == Schema.Type.RECORD =>
            val typedRead = read[java.util.AbstractList[GenericRecord]](f) _
            val xm = typedRead(x).asScala.toList
            val ym = typedRead(y).asScala.toList
            Eq.catsKernelEqForList[GenericRecord](this).eqv(xm, ym)
          case Schema.Type.MAP if fs.getValueType.getType == Schema.Type.RECORD =>
            val typedRead = read[java.util.Map[CharSequence, GenericRecord]](f) _
            val xm = typedRead(x).asScala.map { case (k, v) => k.toString -> v }.toMap
            val ym = typedRead(y).asScala.map { case (k, v) => k.toString -> v }.toMap
            Eq.catsKernelEqForMap[String, GenericRecord](this).eqv(xm, ym)
          case Schema.Type.ARRAY =>
            val typedRead = read[java.util.AbstractList[_]](f) _
            val xm = typedRead(x).asScala.toList
            val ym = typedRead(y).asScala.toList
            xm == ym
          case Schema.Type.MAP =>
            val typedRead = read[java.util.Map[CharSequence, _]](f) _
            val xm = typedRead(x).asScala.map { case (k, v) => k.toString -> v }.toMap
            val ym = typedRead(y).asScala.map { case (k, v) => k.toString -> v }.toMap
            xm == ym
          case Schema.Type.STRING if f.getProp(GenericData.STRING_PROP) != "String" =>
            read[CharSequence](f)(x).toString == read[CharSequence](f)(y).toString
          case _ =>
            read[AnyRef](f)(x) == read[AnyRef](f)(y)
        }
      }
    }
  }

  private def test[T: Arbitrary: ClassTag](implicit
    t: AvroType[T],
    eq: Eq[T]
  ): Unit = {
    val tpe = ensureSerializable(t)
    val copier = new Copier(tpe.schema)
    property(className[T]) {
      Prop.forAll { (t: T) =>
        val r = tpe(t)
        val rCopy = copier(r)
        val copy = tpe(rCopy)
        Prop.all(eq.eqv(t, copy), eqr.eqv(r, rCopy))
      }
    }
  }

  private def assertLogicalType(
    schema: Schema,
    fieldName: String,
    logicalType: String,
    backwardsCompatible: Boolean = true
  ): Unit = {
    // validate that the string schema contains the correct logical type
    val jf = new JsonFactory
    val om = new ObjectMapper(jf)
    val tree = om.readTree[JsonNode](jf.createParser(schema.toString()))
    val sf = tree.get("fields").elements().asScala.find(_.get("name").asText() == fieldName)
    assert(sf.isDefined, s"schema field $fieldName is undefined")
    val slt = sf.flatMap(f => Try(f.get("type").get("logicalType").asText()).toOption)
    assert(
      slt.contains(logicalType),
      s"schema field $fieldName has logicalType ${slt.getOrElse("null")}, should be $logicalType"
    )

    // validate that the parsed schema contains the correct logical type if this type is backwards compatible or if we're testing with a recent avro
    val compatibilityMode = sys.props.get("avro.version").contains("1.8.2")
    val shouldTestSchema = backwardsCompatible || (!compatibilityMode)
    if (shouldTestSchema) {
      val f = schema.getFields.asScala.find(_.name() == fieldName)
      assert(f.isDefined, s"field $fieldName is undefined")
      val lt = f.flatMap(f => Try(f.schema().getLogicalType.getName).toOption)
      assert(
        lt.contains(logicalType),
        s"field $fieldName has logicalType ${lt.getOrElse("null")}, should be $logicalType"
      )
    }
  }

  implicit val arbBigDecimal: Arbitrary[BigDecimal] = Arbitrary {
    // bq logical type has precision of 38 and scale of 9
    val max = BigInt(10).pow(38) - 1
    Gen.choose(-max, max).map(BigDecimal(_, 9))
  }
  implicit val arbCountryCode: Arbitrary[CountryCode] = Arbitrary(
    Gen.oneOf("US", "UK", "CA", "MX").map(CountryCode.apply)
  )

  implicit val afUri: AvroField[URI] =
    AvroField.from[String](URI.create)(_.toString)
  implicit val afDuration: AvroField[Duration] =
    AvroField.from[Long](Duration.ofMillis)(_.toMillis)
  implicit val afCountryCode: AvroField[CountryCode] =
    AvroField.fixed[CountryCode](2)(bs => CountryCode(new String(bs)))(cc => cc.code.getBytes)

  test[Integers]
  test[Floats]
  test[Required]
  test[Nullable]
  test[Repeated]
  test[Nested]
  test[Unsafe]
  test[Collections]
  test[MoreCollections]
  test[Enums]
  test[UnsafeEnums]
  test[Custom]
  test[AvroTypes]

  test("String vs CharSequence with avro.java.string property") {
    val at: AvroType[AvroTypes] = AvroType[AvroTypes]
    val copier = new Copier(at.schema)
    // original uses String as CharSequence implementation
    val original = AvroTypes(
      "String",
      "CharSequence",
      Array.emptyByteArray,
      ByteBuffer.allocate(0),
      null
    )
    val copy = at.from(copier.apply(at.to(original)))
    // copy uses avro Utf8 as CharSequence implementation
    assert(original != copy)
    assert(copy.str.isInstanceOf[String])
    assert(copy.cs.isInstanceOf[Utf8])
  }

  test("AnyVal") {
    implicit val at: AvroType[HasValueClass] = AvroType[HasValueClass]
    test[HasValueClass]

    assert(at.schema.getField("vc").schema().getType == Schema.Type.STRING)

    val record = at(HasValueClass(ValueClass("String")))
    assert(record.get("vc") == "String")
  }

  test[MapPrimitive]
  test[MapNested]

  {
    // generate unsigned-int duration values
    implicit val arbAvroDuration: Arbitrary[AvroDuration] = Arbitrary {
      for {
        months <- Gen.chooseNum(0L, 0xffffffffL)
        days <- Gen.chooseNum(0L, 0xffffffffL)
        millis <- Gen.chooseNum(0L, 0xffffffffL)
      } yield AvroDuration(months, days, millis)
    }

    implicit val afAvroDuration: AvroField[AvroDuration] = AvroField.from[(Long, Long, Long)] {
      case (months, days, millis) => AvroDuration(months, days, millis)
    }(d => (d.months, d.days, d.millis))(AvroField.afDuration)

    test[Logical]

    test("LogicalTypes") {
      val schema = AvroType[Logical].schema
      assertLogicalType(schema, "ld", "date")
      assertLogicalType(schema, "jld", "date")
      assertLogicalType(schema, "d", "duration")
    }
  }

  {
    import magnolify.avro.logical.micros._
    test[LogicalMicros]

    test("MicrosLogicalTypes") {
      val schema = AvroType[LogicalMicros].schema
      assertLogicalType(schema, "i", "timestamp-micros")
      assertLogicalType(schema, "ldt", "local-timestamp-micros", backwardsCompatible = false)
      assertLogicalType(schema, "lt", "time-micros")
      assertLogicalType(schema, "jdt", "timestamp-micros")
      assertLogicalType(schema, "jlt", "time-micros")
    }
  }

  {
    import magnolify.avro.logical.millis._
    test[LogicalMillis]

    test("MilliLogicalTypes") {
      val schema = AvroType[LogicalMillis].schema
      assertLogicalType(schema, "i", "timestamp-millis")
      assertLogicalType(schema, "ldt", "local-timestamp-millis", backwardsCompatible = false)
      assertLogicalType(schema, "lt", "time-millis")
      assertLogicalType(schema, "jdt", "timestamp-millis")
      assertLogicalType(schema, "jlt", "time-millis")
    }
  }

  {
    import magnolify.avro.logical.bigquery._
    test[LogicalBigQuery]

    test("BigQueryLogicalTypes") {
      // `registerLogicalTypes()` is necessary to correctly parse a custom logical type from string.
      // if omitted, the returned string schema will be correct, but the logicalType field will be null.
      // this is unfortunately global mutable state
      registerLogicalTypes()

      val schema = AvroType[LogicalBigQuery].schema
      assertLogicalType(schema, "bd", "decimal")
      assertLogicalType(schema, "i", "timestamp-micros")
      assertLogicalType(schema, "lt", "time-micros")
      assertLogicalType(schema, "ldt", "datetime")
      assertLogicalType(schema, "jdt", "timestamp-micros")
      assertLogicalType(schema, "jlt", "time-micros")
      assertLogicalType(schema, "jldt", "datetime")
    }
  }

  test[Fixed]
  test("FixedDoc") {
    val at = ensureSerializable(AvroType[Fixed])
    val schema = at.schema.getField("countryCode").schema
    assertEquals(schema.getName, "CountryCode")
    assertEquals(schema.getNamespace, "magnolify.avro")
    assertEquals(schema.getDoc, "Fixed with doc")
  }

  test("AvroDoc") {
    val at = ensureSerializable(AvroType[AvroDoc])
    val schema = at.schema
    assertEquals(schema.getDoc, "Avro with doc")
    val fields = schema.getFields.asScala
    assert(fields.find(_.name() == "s").exists(_.doc() == "string"))
    assert(fields.find(_.name() == "i").exists(_.doc() == "integers"))
  }

  test("CustomDoc") {
    val at = ensureSerializable(AvroType[CustomDoc])
    val schema = at.schema
    assertEquals(schema.getDoc, """{"doc": "Avro with doc", "path": "/path/to/my/data"}""")
    val fields = schema.getFields.asScala
    val ds = """{"doc": "string", "since": "2020-01-01"}"""
    val di = """{"doc": "integers", "since": "2020-02-01"}"""
    assert(fields.find(_.name() == "s").exists(_.doc() == ds))
    assert(fields.find(_.name() == "i").exists(_.doc() == di))
  }

  testFail(AvroType[DoubleRecordDoc])(
    "More than one @doc annotation: magnolify.avro.DoubleRecordDoc"
  )
  testFail(AvroType[DoubleFieldDoc])(
    "More than one @doc annotation: magnolify.avro.DoubleFieldDoc#i"
  )

  test("EnumDoc") {
    val at = ensureSerializable(AvroType[EnumDoc])
    assertEquals(at.schema.getField("p").schema().getDoc, "Avro enum")
  }

  {
    import magnolify.avro.logical.bigquery._
    test("DefaultInner") {
      val at = ensureSerializable(AvroType[DefaultInner])
      assertEquals(at(new GenericRecordBuilder(at.schema).build()), DefaultInner())
      val inner = DefaultInner(
        2,
        Some(2),
        Right("2"),
        List(2, 2),
        Map("b" -> 2),
        JavaEnums.Color.GREEN,
        ScalaEnums.Color.Green,
        BigDecimal(222.222222222), // scale is 9
        UUID.fromString("22223333-abcd-abcd-abcd-222233334444"),
        Instant.ofEpochSecond(22334455L),
        LocalDate.ofEpochDay(2233),
        LocalTime.of(2, 3, 4),
        LocalDateTime.of(2002, 3, 4, 5, 6, 7)
      )
      assertEquals(at(at(inner)), inner)
    }

    test("DefaultOuter") {
      val at = ensureSerializable(AvroType[DefaultOuter])
      assertEquals(at(new GenericRecordBuilder(at.schema).build()), DefaultOuter())
      val inner = DefaultInner(
        3,
        Some(3),
        Right("3"),
        List(3, 3),
        Map("c" -> 3),
        JavaEnums.Color.BLUE,
        ScalaEnums.Color.Blue,
        BigDecimal(333.333333333), // scale is 9
        UUID.fromString("33334444-abcd-abcd-abcd-333344445555"),
        Instant.ofEpochSecond(33445566L),
        LocalDate.ofEpochDay(3344),
        LocalTime.of(3, 4, 5),
        LocalDateTime.of(2003, 4, 5, 6, 7, 8)
      )
      val outer = DefaultOuter(inner, Some(inner))
      assertEquals(at(at(outer)), outer)
    }
  }

  test("DefaultBytes") {
    val at = ensureSerializable(AvroType[DefaultBytes])
    assertEquals(at(new GenericRecordBuilder(at.schema).build()), DefaultBytes())
  }

  testFail(AvroType[DefaultSome])("Option[T] can only default to None")
  testFail(AvroType[DefaultEither])("Either[A, B] can only default to Left[A]")

  {
    implicit val at: AvroType[LowerCamel] = AvroType[LowerCamel](CaseMapper(_.toUpperCase))
    test[LowerCamel]

    test("LowerCamel mapping") {
      val schema = at.schema
      val fields = LowerCamel.fields.map(_.toUpperCase)
      assertEquals(schema.getFields.asScala.map(_.name()).toSeq, fields)
      assertEquals(
        schema.getField("INNERFIELD").schema().getFields.asScala.map(_.name()).toSeq,
        Seq("INNERFIRST")
      )

      val record = at(LowerCamel.default)
      assert(fields.forall(record.get(_) != null))
      assert(record.get("INNERFIELD").asInstanceOf[GenericRecord].get("INNERFIRST") != null)
    }
  }

  test("BigDecimal") {
    implicit val afBigDecimal: AvroField[BigDecimal] = AvroField.bigDecimal(38, 9)
    val at: AvroType[BigDec] = AvroType[BigDec]

    val bigInt = "1234567890123456789012345678901234567890"
    val msg1 = "requirement failed: " +
      s"Cannot encode BigDecimal $bigInt: precision 49 > 38 after set scale from 0 to 9"
    interceptMessage[IllegalArgumentException](msg1) {
      at(BigDec(BigDecimal(bigInt)))
    }

    val msg2 = "requirement failed: " +
      s"Cannot decode BigDecimal ${BigDecimal(BigInt(bigInt), 9)}: precision 40 > 38"
    val record = new GenericRecordBuilder(at.schema)
      .set("bd", ByteBuffer.wrap(BigDecimal(bigInt).underlying().unscaledValue().toByteArray))
      .build()
    interceptMessage[IllegalArgumentException](msg2) {
      at(record)
    }

    interceptMessage[ArithmeticException]("Rounding necessary") {
      at(BigDec(BigDecimal("3.14159265358979323846")))
    }
  }

  test("Copy bytes when reading from byte buffer") {
    def toBytes[T](values: Seq[T])(implicit at: AvroType[T]): Array[Byte] = {
      val bytes = new ByteArrayOutputStream()
      val datumWriter = new GenericDatumWriter[GenericRecord](at.schema)
      val binaryEncoder = EncoderFactory.get().binaryEncoder(bytes, null)
      values.map(at.apply).foreach(datumWriter.write(_, binaryEncoder))
      binaryEncoder.flush()
      bytes.toByteArray
    }

    def fromBytes[T](bytes: Array[Byte])(implicit at: AvroType[T]): Seq[T] = {
      val datumReader = new GenericDatumReader[GenericRecord](at.schema)
      val decoder = DecoderFactory.get().binaryDecoder(bytes, null)
      // Seq.unfold is not available in Scala 2.12 / collection-compat yet
      val b = Seq.newBuilder[T]
      var datum: GenericRecord = null
      while (!decoder.isEnd) {
        datum = datumReader.read(datum, decoder)
        b += at.from(datum)
      }
      b.result()
    }

    val expected = List(DefaultBytes(Array(1, 2)), DefaultBytes(Array(3, 4)))
    val actual = fromBytes[DefaultBytes](toBytes(expected))
    assertEquals(actual, expected)
  }
}

case class Unsafe(b: Byte, c: Char, s: Short)
case class AvroTypes(str: String, cs: CharSequence, ba: Array[Byte], bb: ByteBuffer, n: Null)
case class MapPrimitive(strMap: Map[String, Int], charSeqMap: Map[CharSequence, Int])
case class MapNested(m: Map[String, Nested], charSeqMap: Map[CharSequence, Nested])

case class AvroDuration(months: Long, days: Long, millis: Long)
case class Logical(u: UUID, ld: LocalDate, jld: joda.LocalDate, d: AvroDuration)
case class LogicalMicros(
  i: Instant,
  lt: LocalTime,
  ldt: LocalDateTime,
  jdt: joda.DateTime,
  jlt: joda.LocalTime
)
case class LogicalMillis(
  i: Instant,
  lt: LocalTime,
  ldt: LocalDateTime,
  jdt: joda.DateTime,
  jlt: joda.LocalTime
)
case class LogicalBigQuery(
  bd: BigDecimal,
  i: Instant,
  lt: LocalTime,
  ldt: LocalDateTime,
  jdt: joda.DateTime,
  jlt: joda.LocalTime,
  jldt: joda.LocalDateTime
)
case class BigDec(bd: BigDecimal)

@doc("Fixed with doc")
case class CountryCode(code: String)
case class Fixed(countryCode: CountryCode)

@doc("Avro with doc")
case class AvroDoc(@doc("string") s: String, @doc("integers") i: Integers)

class datasetDoc(doc: String, path: String) extends doc(s"""{"doc": "$doc", "path": "$path"}""")

class fieldDoc(doc: String, since: LocalDate)
    extends doc(
      s"""{"doc": "$doc", "since": "${since.format(DateTimeFormatter.ofPattern("YYYY-MM-dd"))}"}"""
    )

@datasetDoc("Avro with doc", "/path/to/my/data")
case class CustomDoc(
  @fieldDoc("string", LocalDate.of(2020, 1, 1)) s: String,
  @fieldDoc("integers", LocalDate.of(2020, 2, 1)) i: Integers
)

@doc("doc1")
@doc("doc2")
case class DoubleRecordDoc(i: Int)
case class DoubleFieldDoc(@doc("doc1") @doc("doc2") i: Int)

case class DefaultInner(
  i: Int = 1,
  o: Option[Int] = None,
  e: Either[Int, String] = Left(1),
  l: List[Int] = List(1, 1),
  m: Map[String, Int] = Map("a" -> 1),
  je: JavaEnums.Color = JavaEnums.Color.RED,
  se: ScalaEnums.Color.Type = ScalaEnums.Color.Red,
  bd: BigDecimal = BigDecimal(111.111111111), // scale is 9
  u: UUID = UUID.fromString("11112222-abcd-abcd-abcd-111122223333"),
  ts: Instant = Instant.ofEpochSecond(11223344),
  ld: LocalDate = LocalDate.ofEpochDay(1122),
  lt: LocalTime = LocalTime.of(1, 2, 3),
  ldt: LocalDateTime = LocalDateTime.of(2001, 2, 3, 4, 5, 6)
)
case class DefaultOuter(
  i: DefaultInner = DefaultInner(
    2,
    None,
    Left(2),
    List(2, 2),
    Map("b" -> 2),
    JavaEnums.Color.GREEN,
    ScalaEnums.Color.Green,
    BigDecimal(222.222222222), // scale is 9
    UUID.fromString("22223333-abcd-abcd-abcd-222233334444"),
    Instant.ofEpochSecond(22334455L),
    LocalDate.ofEpochDay(2233),
    LocalTime.of(2, 3, 4),
    LocalDateTime.of(2002, 3, 4, 5, 6, 7)
  ),
  o: Option[DefaultInner] = None
)

case class DefaultSome(o: Option[Int] = Some(1))
case class DefaultEither(e: Either[Int, String] = Right("1"))

case class DefaultBytes(
  a: Array[Byte] = Array(2, 2)
  // ByteBuffer is not serializable and can't be used as default value
  // bb: ByteBuffer = ByteBuffer.allocate(2).put(2.toByte).put(2.toByte)
) {

  override def hashCode(): Int =
    util.Arrays.hashCode(a)

  override def equals(obj: Any): Boolean = obj match {
    case that: DefaultBytes => Objects.deepEquals(this.a, that.a)
    case _                  => false
  }

}

@doc("Avro enum")
object Pet extends Enumeration {
  type Type = Value
  val Cat, Dog = Value
}
case class EnumDoc(p: Pet.Type)

private class Copier(private val schema: Schema) {
  private val encoder = EncoderFactory.get
  private val decoder = DecoderFactory.get

  private val datumWriter = new GenericDatumWriter[GenericRecord](schema)
  private val datumReader = new GenericDatumReader[GenericRecord](schema)

  def apply(r: GenericRecord): GenericRecord = {
    val baos = new ByteArrayOutputStream()
    datumWriter.write(r, encoder.directBinaryEncoder(baos, null))
    datumReader.read(
      null,
      decoder.directBinaryDecoder(new ByteArrayInputStream(baos.toByteArray), null)
    )
  }
}
