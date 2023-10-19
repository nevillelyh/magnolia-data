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

import java.{lang => jl, util => ju}

import com.google.protobuf.ByteString
import magnolia1._
import magnolify.shared._
import magnolify.shims.FactoryCompat
import org.tensorflow.metadata.v0.{Annotation, Feature => FeatureSchema, FeatureType, Schema}
import org.tensorflow.proto.example._
import scala.annotation.{implicitNotFound, StaticAnnotation}
import scala.collection.concurrent
import scala.jdk.CollectionConverters._
import scala.collection.compat._

class doc(msg: String) extends StaticAnnotation with Serializable {
  override def toString: String = msg
}

sealed trait ExampleType[T] extends Converter[T, Example, Example.Builder] {
  def schema: Schema
  def apply(v: Example): T = from(v)
  def apply(v: T): Example = to(v).build()
}

object ExampleType {
  implicit def apply[T: ExampleField]: ExampleType[T] = ExampleType(CaseMapper.identity)

  def apply[T](cm: CaseMapper)(implicit f: ExampleField[T]): ExampleType[T] = f match {
    case r: ExampleField.Record[_] =>
      new ExampleType[T] {
        @transient override lazy val schema: Schema = r.schema(cm)
        override def from(v: Example): T = r.get(v.getFeatures, null)(cm).get
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

  def get(f: Features, k: String)(cm: CaseMapper): Value[T]
  def put(f: Features.Builder, k: String, v: T)(cm: CaseMapper): Features.Builder
}

object ExampleField {
  sealed trait Primitive[T] extends ExampleField[T] {
    type ValueT
    def fromFeature(v: Feature): ju.List[T]
    def toFeature(v: Iterable[T]): Feature

    override def get(f: Features, k: String)(cm: CaseMapper): Value[T] = {
      val feature = f.getFeatureOrDefault(k, null)
      if (feature == null) {
        Value.None
      } else {
        val values = fromFeature(feature)
        require(values.size() == 1)
        Value.Some(values.get(0))
      }
    }

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
        override def get(f: Features, k: String)(cm: CaseMapper): Value[T] =
          tc.get(f, k)(cm).map(x => caseClass.construct(_ => x))
        override def put(f: Features.Builder, k: String, v: T)(cm: CaseMapper): Features.Builder =
          tc.put(f, k, p.dereference(v))(cm)
      }
    } else {
      new Record[T] {
        private def key(prefix: String, label: String): String =
          if (prefix == null) label else s"$prefix.$label"

        override def get(f: Features, k: String)(cm: CaseMapper): Value[T] = {
          var fallback = true
          val r = caseClass.construct { p =>
            val fieldKey = key(k, cm.map(p.label))
            val fieldValue = p.typeclass.get(f, fieldKey)(cm)
            if (fieldValue.isSome) {
              fallback = false
            }
            fieldValue.getOrElse(p.default)
          }
          // result is default if all fields are default
          if (fallback) Value.Default(r) else Value.Some(r)
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
      override def get(f: Features, k: String)(cm: CaseMapper): Value[Option[T]] =
        if (f.containsFeature(k) || f.getFeatureMap.keySet().asScala.exists(_.startsWith(s"$k."))) {
          ef.get(f, k)(cm).toOption
        } else {
          Value.Default(None)
        }

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
    override def get(f: Features, k: String)(cm: CaseMapper): Value[C[T]] = {
      val v = f.getFeatureOrDefault(k, null)
      if (v == null) Value.Default(fc.newBuilder.result())
      else Value.Some(fc.fromSpecific(ef.fromFeature(v).asScala))
    }

    override def put(f: Features.Builder, k: String, v: C[T])(cm: CaseMapper): Features.Builder =
      if (v.isEmpty) f else f.putFeature(k, ef.toFeature(v))

    override protected def buildSchema(cm: CaseMapper): Schema =
      ef.buildSchema(cm)
  }
}
