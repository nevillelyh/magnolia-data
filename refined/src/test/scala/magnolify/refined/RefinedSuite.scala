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

package magnolify.refined

import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.auto._
import eu.timepit.refined.boolean._
import eu.timepit.refined.numeric._
import eu.timepit.refined.string._
import magnolify.guava.BytesSink
import magnolify.guava.semiauto.FunnelDerivation
import magnolify.test._

object RefinedSuite {
  type Percent = Int Refined Interval.Closed[W.`0`.T, W.`100`.T]
  type Probability = Double Refined Interval.Closed[W.`0`.T, W.`1`.T]
  type UUID = String Refined Uuid
  type URL = String Refined Url
  type Country = String Refined MatchesRegex[W.`"[A-Z]{2}"`.T]

  case class Record(
    uuid: UUID,
    pct: Percent,
    prob: Probability,
    url: Option[URL],
    countries: List[Country]
  )

  case class FunnelRecordRefined(
    uuid: UUID,
    pct: Percent,
    url: Option[URL],
    countries: List[Country]
  )
  case class FunnelRecord(uuid: String, pct: Int, url: Option[String], countries: List[String])

  case class BigtableRecord(uuid: UUID, pct: Percent, prob: Probability, url: Option[URL])

  type EmptyString = MatchesRegex[W.`"^$"`.T]
  type ProtoUuid = String Refined Or[Uuid, EmptyString]
  type ProtoUrl = String Refined Or[Url, EmptyString]
  type ProtoCountry = String Refined Or[MatchesRegex[W.`"[A-Z]{2}"`.T], EmptyString]
  case class ProtoRequired(b: Boolean, i: Percent, s: ProtoUuid)
  case class ProtoNullable(b: Option[Boolean], i: Option[Percent], s: Option[ProtoUrl])
  case class ProtoRepeated(b: List[Boolean], i: List[Percent], s: List[ProtoCountry])
}
class RefinedSuite extends MagnolifySuite {
  import RefinedSuite._

  private val record = Record(
    "1234abcd-abba-dead-beef-9876543210ab",
    42,
    0.25,
    Some("https://www.spotify.com"),
    List("US", "UK")
  )
  private val errMsg = "Url predicate failed: URI is not absolute"

  test("Guava") {
    import magnolify.guava.auto._
    import magnolify.refined.guava._
    val f1 = ensureSerializable(FunnelDerivation[FunnelRecordRefined])
    val f2 = ensureSerializable(FunnelDerivation[FunnelRecord])

    val r1 = FunnelRecordRefined(record.uuid, record.pct, record.url, record.countries)
    val r2 = FunnelRecord(
      record.uuid.value,
      record.pct.value,
      record.url.map(_.value),
      record.countries.map(_.value)
    )

    val s1 = new BytesSink
    val s2 = new BytesSink
    f1.funnel(r1, s1)
    f2.funnel(r2, s2)
    assertEquals(s1.toBytes.toList, s2.toBytes.toList)
  }

  test("AvroType") {
    import magnolify.avro._
    import magnolify.refined.avro._
    val tpe = ensureSerializable(AvroType[Record])
    val gr = tpe(record)
    assertEquals(tpe(gr), record)

    gr.put("url", "foo")
    interceptMessage[IllegalArgumentException](errMsg)(tpe(gr))
  }

  test("TableRowType") {
    import magnolify.bigquery._
    import magnolify.bigquery.unsafe._
    import magnolify.refined.bigquery._
    val tpe = ensureSerializable(TableRowType[Record])
    val tr = tpe(record)
    assertEquals(tpe(tr), record)

    tr.put("url", "foo")
    interceptMessage[IllegalArgumentException](errMsg)(tpe(tr))
  }

  test("BigtableType") {
    import com.google.protobuf.ByteString
    import magnolify.bigtable._
    import magnolify.refined.bigtable._
    val tpe = ensureSerializable(BigtableType[BigtableRecord])
    val btRecord = BigtableRecord(record.uuid, record.pct, record.prob, record.url)
    val columnFamily = "cf"
    val key = "key"
    val mutations = tpe(btRecord, columnFamily)
    val good = BigtableType.mutationsToRow(ByteString.copyFromUtf8(key), mutations)
    assertEquals(tpe(good, columnFamily), btRecord)

    val badMutations = mutations.map { m =>
      m.getSetCell.getColumnQualifier.toStringUtf8 match {
        case "url" =>
          m.toBuilder
            .setSetCell(m.getSetCell.toBuilder.setValue(ByteString.copyFromUtf8("foo")))
            .build()
        case _ => m
      }
    }
    val bad = BigtableType.mutationsToRow(ByteString.copyFromUtf8(key), badMutations)
    interceptMessage[IllegalArgumentException](errMsg)(tpe(bad, columnFamily))
  }

  test("datastore") {
    import magnolify.datastore._
    import magnolify.datastore.unsafe._
    import magnolify.refined.datastore._
    import com.google.datastore.v1.client.DatastoreHelper.makeValue
    val tpe = ensureSerializable(EntityType[Record])
    val good = tpe(record)
    assertEquals(tpe(good), record)

    val bad = good.toBuilder.putProperties("url", makeValue("foo").build()).build()
    interceptMessage[IllegalArgumentException](errMsg)(tpe(bad))
  }

  test("protobuf") {
    import magnolify.protobuf._
    import magnolify.protobuf.Proto3._
    import magnolify.refined.protobuf._
    val tpe1 = ensureSerializable(ProtobufType[ProtoRequired, RequiredP3])
    val required = ProtoRequired(
      b = true,
      i = record.pct,
      s = refineV.unsafeFrom(record.uuid.value)
    )
    assertEquals(tpe1(tpe1(required)), required)

    val tpe2 = ensureSerializable(ProtobufType[ProtoNullable, NullableP3])
    val nullable = ProtoNullable(
      b = Some(true),
      i = Some(record.pct),
      s = Some(refineV.unsafeFrom(record.url.get.value))
    )
    assertEquals(tpe2(tpe2(nullable)), nullable)

    val tpe3 = ensureSerializable(ProtobufType[ProtoRepeated, RepeatedP3])
    val repeated = ProtoRepeated(
      b = List(true),
      i = List(record.pct),
      s = List(refineV.unsafeFrom("US"), refineV.unsafeFrom("UK"))
    )
    assertEquals(tpe3(tpe3(repeated)), repeated)

    val bad = NullableP3.newBuilder().setB(true).setI(42).setS("foo").build()
    val msg = """Both predicates of (isValidUrl("foo") || "foo".matches("^$")) failed. """ +
      """Left: Url predicate failed: URI is not absolute """ +
      """Right: Predicate failed: "foo".matches("^$")."""
    interceptMessage[IllegalArgumentException](msg)(tpe2(bad))
  }

  test("tensorflow") {
    import magnolify.tensorflow._
    import magnolify.tensorflow.unsafe._
    import magnolify.refined.tensorflow._
    import com.google.protobuf.ByteString
    import org.tensorflow.proto.{BytesList, Feature}
    val tpe = ensureSerializable(ExampleType[Record])
    val good = tpe(record)
    assertEquals(tpe(good), record)

    val badFeatures = good.getFeatures.toBuilder
      .putFeature(
        "url",
        Feature
          .newBuilder()
          .setBytesList(BytesList.newBuilder().addValue(ByteString.copyFromUtf8("foo")))
          .build()
      )
      .build()
    val bad = good.toBuilder.setFeatures(badFeatures).build()
    interceptMessage[IllegalArgumentException](errMsg)(tpe(bad))
  }
}
