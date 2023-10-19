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

package magnolify.bigquery

import java.{util => ju}
import com.google.api.services.bigquery.model.{TableFieldSchema, TableRow, TableSchema}
import com.google.common.io.BaseEncoding
import magnolia1._
import magnolify.shared.{CaseMapper, Converter}
import magnolify.shims.FactoryCompat

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
import scala.collection.concurrent
import scala.annotation.{implicitNotFound, StaticAnnotation}
import scala.jdk.CollectionConverters._
import scala.collection.compat._

class description(description: String) extends StaticAnnotation with Serializable {
  override def toString: String = description
}

sealed trait TableRowType[T] extends Converter[T, TableRow, TableRow] {
  def schema: TableSchema
  def description: String
  def selectedFields: Seq[String]

  def apply(v: TableRow): T = from(v)
  def apply(v: T): TableRow = to(v)
}

object TableRowType {

  implicit def apply[T: TableRowField]: TableRowType[T] = TableRowType(CaseMapper.identity)

  def apply[T](cm: CaseMapper)(implicit f: TableRowField[T]): TableRowType[T] = {
    f match {
      case r: TableRowField.Record[_] =>
        r.fieldSchema(cm) // fail fast on bad annotations
        new TableRowType[T] {
          private val caseMapper: CaseMapper = cm
          @transient override lazy val schema: TableSchema =
            new TableSchema().setFields(r.fieldSchema(caseMapper).getFields)

          override val selectedFields: Seq[String] = r.fields(cm)
          override val description: String = r.fieldSchema(caseMapper).getDescription
          override def from(v: TableRow): T = r.from(v)(caseMapper)
          override def to(v: T): TableRow = r.to(v)(caseMapper)
        }
      case _ =>
        throw new IllegalArgumentException(s"TableRowType can only be created from Record. Got $f")
    }
  }
}

sealed trait TableRowField[T] extends Serializable {
  type FromT
  type ToT

  @transient private lazy val schemaCache: concurrent.Map[ju.UUID, TableFieldSchema] =
    concurrent.TrieMap.empty

  protected def buildSchema(cm: CaseMapper): TableFieldSchema
  def fieldSchema(cm: CaseMapper): TableFieldSchema =
    schemaCache.getOrElseUpdate(cm.uuid, buildSchema(cm))

  def from(v: FromT)(cm: CaseMapper): T
  def to(v: T)(cm: CaseMapper): ToT

  def fromAny(v: Any)(cm: CaseMapper): T = from(v.asInstanceOf[FromT])(cm)
}

object TableRowField {
  sealed trait Aux[T, From, To] extends TableRowField[T] {
    override type FromT = From
    override type ToT = To
  }

  sealed trait Generic[T] extends Aux[T, Any, Any]
  sealed trait Record[T] extends Aux[T, ju.Map[String, AnyRef], TableRow] {
    def fields(cm: CaseMapper): Seq[String]
  }

  // ////////////////////////////////////////////////
  type Typeclass[T] = TableRowField[T]

  def join[T](caseClass: CaseClass[Typeclass, T]): TableRowField[T] = {
    if (caseClass.isValueClass) {
      val p = caseClass.parameters.head
      val tc = p.typeclass
      new TableRowField[T] {
        override type FromT = tc.FromT
        override type ToT = tc.ToT
        override protected def buildSchema(cm: CaseMapper): TableFieldSchema = tc.buildSchema(cm)
        override def from(v: FromT)(cm: CaseMapper): T = caseClass.construct(_ => tc.from(v)(cm))
        override def to(v: T)(cm: CaseMapper): ToT = tc.to(p.dereference(v))(cm)
      }
    } else {
      new Record[T] {
        override protected def buildSchema(cm: CaseMapper): TableFieldSchema = {
          // do not use a scala wrapper in the schema, so clone() works
          val fields = new ju.ArrayList[TableFieldSchema](caseClass.parameters.size)
          caseClass.parameters.foreach { p =>
            val f = p.typeclass
              .fieldSchema(cm)
              .clone()
              .setName(cm.map(p.label))
              .setDescription(
                getDescription(p.annotations, s"${caseClass.typeName.full}#${p.label}")
              )
            fields.add(f)
          }

          new TableFieldSchema()
            .setType("STRUCT")
            .setMode("REQUIRED")
            .setDescription(getDescription(caseClass.annotations, caseClass.typeName.full))
            .setFields(fields)
        }

        override def fields(cm: CaseMapper): Seq[String] = {
          for {
            p <- caseClass.parameters
            n = cm.map(p.label)
            f <- p.typeclass match {
              case r: Record[_] => r.fields(cm).map(child => n + "." + child)
              case _            => Seq(n)
            }
          } yield f
        }

        override def from(v: ju.Map[String, AnyRef])(cm: CaseMapper): T =
          caseClass.construct { p =>
            val f = v.get(cm.map(p.label))
            if (f == null && p.default.isDefined) {
              p.default.get
            } else {
              p.typeclass.fromAny(f)(cm)
            }
          }

        override def to(v: T)(cm: CaseMapper): TableRow =
          caseClass.parameters.foldLeft(new TableRow) { (tr, p) =>
            val f = p.typeclass.to(p.dereference(v))(cm)
            if (f == null) tr else tr.set(cm.map(p.label), f)
          }

        private def getDescription(annotations: Seq[Any], name: String): String = {
          val descs = annotations.collect { case d: description => d.toString }
          require(descs.size <= 1, s"More than one @description annotation: $name")
          descs.headOption.orNull
        }
      }
    }
  }

  @implicitNotFound("Cannot derive TableRowField for sealed trait")
  private sealed trait Dispatchable[T]
  def split[T: Dispatchable](sealedTrait: SealedTrait[Typeclass, T]): TableRowField[T] = ???

  implicit def gen[T]: TableRowField[T] = macro Magnolia.gen[T]

  // ////////////////////////////////////////////////

  def apply[T](implicit f: TableRowField[T]): TableRowField[T] = f

  def from[T]: FromWord[T] = new FromWord[T]

  class FromWord[T] {
    def apply[U](f: T => U)(g: U => T)(implicit trf: TableRowField[T]): TableRowField[U] =
      new TableRowField[U] {
        override type FromT = trf.FromT
        override type ToT = trf.ToT

        override protected def buildSchema(cm: CaseMapper): TableFieldSchema =
          trf.fieldSchema(cm)
        override def from(v: FromT)(cm: CaseMapper): U = f(trf.from(v)(cm))
        override def to(v: U)(cm: CaseMapper): ToT = trf.to(g(v))(cm)
      }
  }

  // ////////////////////////////////////////////////

  private def at[T](tpe: String)(f: Any => T)(g: T => Any): TableRowField[T] = new Generic[T] {
    override protected def buildSchema(cm: CaseMapper): TableFieldSchema =
      new TableFieldSchema().setType(tpe).setMode("REQUIRED")
    override def from(v: Any)(cm: CaseMapper): T = f(v)
    override def to(v: T)(cm: CaseMapper): Any = g(v)
  }

  implicit val trfBool: TableRowField[Boolean] = at[Boolean]("BOOL")(_.toString.toBoolean)(identity)
  implicit val trfLong: TableRowField[Long] = at[Long]("INT64")(_.toString.toLong)(identity)
  implicit val trfDouble: TableRowField[Double] =
    at[Double]("FLOAT64")(_.toString.toDouble)(identity)
  implicit val trfString: TableRowField[String] = at[String]("STRING")(_.toString)(identity)
  implicit val trfNumeric: TableRowField[BigDecimal] =
    at[BigDecimal]("NUMERIC")(NumericConverter.toBigDecimal)(NumericConverter.fromBigDecimal)

  implicit val trfByteArray: TableRowField[Array[Byte]] =
    at[Array[Byte]]("BYTES")(x => BaseEncoding.base64().decode(x.toString))(x =>
      BaseEncoding.base64().encode(x)
    )

  import TimestampConverter._
  implicit val trfInstant: TableRowField[Instant] = at("TIMESTAMP")(toInstant)(fromInstant)
  implicit val trfDate: TableRowField[LocalDate] = at("DATE")(toLocalDate)(fromLocalDate)
  implicit val trfTime: TableRowField[LocalTime] = at("TIME")(toLocalTime)(fromLocalTime)
  implicit val trfDateTime: TableRowField[LocalDateTime] =
    at("DATETIME")(toLocalDateTime)(fromLocalDateTime)

  implicit def trfOption[T](implicit f: TableRowField[T]): TableRowField[Option[T]] =
    new Aux[Option[T], f.FromT, f.ToT] {
      override protected def buildSchema(cm: CaseMapper): TableFieldSchema =
        f.fieldSchema(cm).clone().setMode("NULLABLE")
      override def from(v: f.FromT)(cm: CaseMapper): Option[T] =
        if (v == null) None else Some(f.from(v)(cm))
      override def to(v: Option[T])(cm: CaseMapper): f.ToT = v match {
        case None    => null.asInstanceOf[f.ToT]
        case Some(x) => f.to(x)(cm)
      }
    }

  implicit def trfIterable[T, C[_]](implicit
    f: TableRowField[T],
    ti: C[T] => Iterable[T],
    fc: FactoryCompat[T, C[T]]
  ): TableRowField[C[T]] =
    new Aux[C[T], ju.List[f.FromT], ju.List[f.ToT]] {
      override protected def buildSchema(cm: CaseMapper): TableFieldSchema =
        f.fieldSchema(cm).clone().setMode("REPEATED")
      override def from(v: ju.List[f.FromT])(cm: CaseMapper): C[T] = {
        val b = fc.newBuilder
        if (v != null) {
          b ++= v.asScala.iterator.map(f.from(_)(cm))
        }
        b.result()
      }

      override def to(v: C[T])(cm: CaseMapper): ju.List[f.ToT] =
        if (v.isEmpty) null else v.iterator.map(f.to(_)(cm)).toList.asJava
    }
}

private object NumericConverter {
  // https://cloud.google.com/bigquery/docs/reference/standard-sql/data-types#numeric-type
  private val MaxPrecision = 38
  private val MaxScale = 9

  def toBigDecimal(v: Any): BigDecimal = BigDecimal(v.toString)
  def fromBigDecimal(v: BigDecimal): Any = {
    require(
      v.precision <= MaxPrecision,
      s"Cannot encode BigDecimal $v: precision ${v.precision} > $MaxPrecision"
    )
    require(v.scale <= MaxScale, s"Cannot encode BigDecimal $v: scale ${v.scale} > $MaxScale")
    v.toString()
  }
}
