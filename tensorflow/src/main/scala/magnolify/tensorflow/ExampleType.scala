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

package magnolify.tensorflow

import com.google.protobuf.ByteString
import magnolia1.*
import magnolify.shared.*
import magnolify.shims.FactoryCompat
import org.tensorflow.metadata.v0.{Annotation, Feature as FeatureSchema, FeatureType, Schema}
import org.tensorflow.proto.*

import java.{lang as jl, util as ju}
import scala.annotation.{implicitNotFound, StaticAnnotation}
import scala.collection.concurrent
import scala.jdk.CollectionConverters.*
import scala.collection.compat.*

class doc(msg: String) extends StaticAnnotation with Serializable {
  override def toString: String = msg
}

sealed trait ExampleType[T] extends Converter[T, Map[String, Feature], Example.Builder] {
  val schema: Schema
  def apply(v: Example): T = from(v.getFeatures.getFeatureMap.asScala.toMap)
  def apply(v: T): Example = to(v).build()
}

object ExampleType {
  implicit def apply[T: ExampleField]: ExampleType[T] = ExampleType(CaseMapper.identity)

  def apply[T](cm: CaseMapper)(implicit f: ExampleField[T]): ExampleType[T] = f match {
    case r: ExampleField.Record[_] =>
      new ExampleType[T] {
        @transient override lazy val schema: Schema = r.schema(cm)
        override def from(v: Map[String, Feature]): T =
          r.get(v, null)(cm)
        override def to(v: T): Example.Builder =
          Example.newBuilder().setFeatures(r.put(Features.newBuilder(), null, v)(cm))
      }
    case _ =>
      throw new IllegalArgumentException(s"ExampleType can only be created from Record. Got $f")
  }
}

sealed trait ExampleField[T] extends Serializable {
  @transient private lazy val schemaCache: concurrent.Map[ju.UUID, Schema] =
    concurrent.TrieMap.empty

  protected def buildSchema(cm: CaseMapper): Schema
  def schema(cm: CaseMapper): Schema =
    schemaCache.getOrElseUpdate(cm.uuid, buildSchema(cm))

  def get(fs: Map[String, Feature], k: String)(cm: CaseMapper): T
  def put(f: Features.Builder, k: String, v: T)(cm: CaseMapper): Features.Builder
}

object ExampleField {
  private def key(prefix: String, label: String): String =
    if (prefix == null) label else s"$prefix.$label"

  private def featureFilter(key: String): (String, Feature) => Boolean = {
    val recordKey = key + "."
    (name: String, _: Feature) => name == key || name.startsWith(recordKey)
  }

  sealed trait Primitive[T] extends ExampleField[T] {
    type ValueT
    def fromFeature(v: Feature): ju.List[T]
    def toFeature(v: Iterable[T]): Feature

    override def get(fs: Map[String, Feature], k: String)(cm: CaseMapper): T =
      fromFeature(fs(k)).ensuring(_.size() == 1).get(0)

    override def put(f: Features.Builder, k: String, v: T)(cm: CaseMapper): Features.Builder =
      f.putFeature(k, toFeature(Iterable(v)))

    override def buildSchema(cm: CaseMapper): Schema =
      Schema.newBuilder().addFeature(featureSchema(cm)).build()

    def featureSchema(cm: CaseMapper): FeatureSchema
  }

  sealed trait Record[T] extends ExampleField[T]

  // ////////////////////////////////////////////////

  type Typeclass[T] = ExampleField[T]

  def join[T](caseClass: CaseClass[Typeclass, T]): ExampleField[T] = {
    if (caseClass.isValueClass) {
      val p = caseClass.parameters.head
      val tc = p.typeclass
      new ExampleField[T] {
        override protected def buildSchema(cm: CaseMapper): Schema =
          tc.buildSchema(cm)
        override def get(fs: Map[String, Feature], k: String)(cm: CaseMapper): T =
          caseClass.construct(_ => tc.get(fs, k)(cm))
        override def put(f: Features.Builder, k: String, v: T)(cm: CaseMapper): Features.Builder =
          tc.put(f, k, p.dereference(v))(cm)
      }
    } else {
      new Record[T] {
        override def get(fs: Map[String, Feature], k: String)(cm: CaseMapper): T = {
          caseClass.construct { p =>
            val fieldKey = key(k, cm.map(p.label))
            val fieldsFeatures = fs.filter(featureFilter(fieldKey).tupled)
            // consider default value only if all fields are missing
            p.default
              .filter(_ => fieldsFeatures.isEmpty)
              .getOrElse(p.typeclass.get(fieldsFeatures, fieldKey)(cm))
          }
        }

        override def put(f: Features.Builder, k: String, v: T)(cm: CaseMapper): Features.Builder =
          caseClass.parameters.foldLeft(f) { (f, p) =>
            val fieldKey = key(k, cm.map(p.label))
            val fieldValue = p.dereference(v)
            p.typeclass.put(f, fieldKey, fieldValue)(cm)
            f
          }

        override protected def buildSchema(cm: CaseMapper): Schema = {
          val sb = Schema.newBuilder()
          getDoc(caseClass.annotations, caseClass.typeName.full).foreach(sb.setAnnotation)
          caseClass.parameters.foldLeft(sb) { (b, p) =>
            val fieldNane = cm.map(p.label)
            val fieldSchema = p.typeclass.schema(cm)
            val fieldFeatures = fieldSchema.getFeatureList.asScala.map { f =>
              val fb = f.toBuilder
              // if schema does not have a name (eg. primitive), use the fieldNane
              // otherwise prepend to the feature name (eg. nested records)
              val fieldKey = if (f.hasName) key(fieldNane, f.getName) else fieldNane
              fb.setName(fieldKey)
              // if field already has a doc, keep it
              // otherwise use the parameter annotation
              val fieldDoc = getDoc(p.annotations, s"${caseClass.typeName.full}#$fieldKey")
              if (!f.hasAnnotation) fieldDoc.foreach(fb.setAnnotation)
              fb.build()
            }.asJava
            b.addAllFeature(fieldFeatures)
            b
          }
          sb.build()
        }
      }
    }
  }

  private def getDoc(annotations: Seq[Any], name: String): Option[Annotation] = {
    val docs = annotations.collect { case d: doc => d.toString }
    require(docs.size <= 1, s"More than one @doc annotation: $name")
    docs.headOption.map(doc => Annotation.newBuilder().addTag(doc).build())
  }

  @implicitNotFound("Cannot derive ExampleField for sealed trait")
  private sealed trait Dispatchable[T]
  def split[T: Dispatchable](sealedTrait: SealedTrait[Typeclass, T]): ExampleField[T] = ???

  implicit def gen[T]: ExampleField[T] = macro Magnolia.gen[T]

  // ////////////////////////////////////////////////

  def apply[T](implicit f: ExampleField[T]): ExampleField[T] = f

  def from[T]: FromWord[T] = new FromWord

  class FromWord[T] {
    def apply[U](f: T => U)(g: U => T)(implicit ef: Primitive[T]): Primitive[U] =
      new Primitive[U] {
        override type ValueT = ef.ValueT
        override def fromFeature(v: Feature): ju.List[U] =
          ef.fromFeature(v).asScala.map(f).asJava
        override def toFeature(v: Iterable[U]): Feature = ef.toFeature(v.map(g))
        override def featureSchema(cm: CaseMapper): FeatureSchema =
          ef.featureSchema(cm)
      }
  }

  implicit val efLong: Primitive[Long] = new Primitive[Long] {
    override type ValueT = jl.Long
    override def fromFeature(v: Feature): ju.List[Long] =
      if (v == null) {
        java.util.Collections.emptyList()
      } else {
        v.getInt64List.getValueList.asInstanceOf[ju.List[Long]]
      }

    override def toFeature(v: Iterable[Long]): Feature =
      Feature
        .newBuilder()
        .setInt64List(Int64List.newBuilder().addAllValue(v.asInstanceOf[Iterable[jl.Long]].asJava))
        .build()

    override def featureSchema(cm: CaseMapper): FeatureSchema =
      FeatureSchema.newBuilder().setType(FeatureType.INT).build()
  }

  implicit val efFloat: Primitive[Float] = new Primitive[Float] {
    override type ValueT = jl.Float
    override def fromFeature(v: Feature): ju.List[Float] =
      if (v == null) {
        java.util.Collections.emptyList()
      } else {
        v.getFloatList.getValueList.asInstanceOf[ju.List[Float]]
      }

    override def toFeature(v: Iterable[Float]): Feature =
      Feature
        .newBuilder()
        .setFloatList(FloatList.newBuilder().addAllValue(v.asInstanceOf[Iterable[jl.Float]].asJava))
        .build()

    override def featureSchema(cm: CaseMapper): FeatureSchema =
      FeatureSchema.newBuilder().setType(FeatureType.FLOAT).build()

  }

  implicit val efByteString: Primitive[ByteString] = new Primitive[ByteString] {
    override type ValueT = ByteString
    override def fromFeature(v: Feature): ju.List[ByteString] =
      if (v == null) {
        java.util.Collections.emptyList()
      } else {
        v.getBytesList.getValueList
      }

    override def toFeature(v: Iterable[ByteString]): Feature =
      Feature
        .newBuilder()
        .setBytesList(BytesList.newBuilder().addAllValue(v.asJava))
        .build()

    override def featureSchema(cm: CaseMapper): FeatureSchema =
      FeatureSchema.newBuilder().setType(FeatureType.BYTES).build()

  }

  implicit val efByteArray: Primitive[Array[Byte]] =
    from[ByteString](_.toByteArray)(ByteString.copyFrom(_))

  implicit def efOption[T](implicit ef: ExampleField[T]): ExampleField[Option[T]] =
    new ExampleField[Option[T]] {
      override def get(fs: Map[String, Feature], k: String)(cm: CaseMapper): Option[T] =
        if (fs.isEmpty) None else Some(ef.get(fs, k)(cm))

      override def put(f: Features.Builder, k: String, v: Option[T])(
        cm: CaseMapper
      ): Features.Builder = v match {
        case None    => f
        case Some(x) => ef.put(f, k, x)(cm)
      }

      override protected def buildSchema(cm: CaseMapper): Schema =
        ef.buildSchema(cm)
    }

  implicit def efIterable[T, C[_]](implicit
    ef: Primitive[T],
    ti: C[T] => Iterable[T],
    fc: FactoryCompat[T, C[T]]
  ): ExampleField[C[T]] = new ExampleField[C[T]] {
    override def get(fs: Map[String, Feature], k: String)(cm: CaseMapper): C[T] =
      if (fs.isEmpty) fc.newBuilder.result() else fc.fromSpecific(ef.fromFeature(fs(k)).asScala)

    override def put(f: Features.Builder, k: String, v: C[T])(cm: CaseMapper): Features.Builder =
      if (v.isEmpty) f else f.putFeature(k, ef.toFeature(v))

    override protected def buildSchema(cm: CaseMapper): Schema =
      ef.buildSchema(cm)
  }
}
