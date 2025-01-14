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

package magnolify.parquet

import org.apache.parquet.io.InvalidRecordException
import org.apache.parquet.schema.PrimitiveType.PrimitiveTypeName
import org.apache.parquet.schema.Type.Repetition
import org.apache.parquet.schema.{
  GroupType,
  LogicalTypeAnnotation,
  MessageType,
  PrimitiveType,
  Type,
  Types
}
import org.slf4j.LoggerFactory
import scala.jdk.CollectionConverters._

private object Schema {
  private lazy val logger = LoggerFactory.getLogger(this.getClass)

  def rename(schema: Type, name: String): Type = {
    if (schema.isPrimitive) {
      val p = schema.asPrimitiveType()
      Types
        .primitive(p.getPrimitiveTypeName, schema.getRepetition)
        .as(schema.getLogicalTypeAnnotation)
        .length(p.getTypeLength)
        .named(name)
    } else {
      schema
        .asGroupType()
        .getFields
        .asScala
        .foldLeft(Types.buildGroup(schema.getRepetition))(_.addField(_))
        .as(schema.getLogicalTypeAnnotation)
        .named(name)
    }
  }

  def setRepetition(schema: Type, repetition: Repetition): Type = {
    require(schema.isRepetition(Repetition.REQUIRED))
    if (schema.isPrimitive) {
      val p = schema.asPrimitiveType()
      Types
        .primitive(p.getPrimitiveTypeName, repetition)
        .as(schema.getLogicalTypeAnnotation)
        .length(p.getTypeLength)
        .named(schema.getName)
    } else {
      schema
        .asGroupType()
        .getFields
        .asScala
        .foldLeft(Types.buildGroup(repetition))(_.addField(_))
        .as(schema.getLogicalTypeAnnotation)
        .named(schema.getName)
    }
  }

  def setLogicalType(schema: Type, lta: LogicalTypeAnnotation): Type = {
    require(schema.isPrimitive)
    val p = schema.asPrimitiveType()
    Types
      .primitive(p.getPrimitiveTypeName, schema.getRepetition)
      .as(lta)
      .length(p.getTypeLength)
      .named(schema.getName)
  }

  def primitive(ptn: PrimitiveTypeName, lta: LogicalTypeAnnotation = null, length: Int = 0): Type =
    Types
      .required(ptn)
      .as(lta)
      .length(length)
      .named(ptn.name())

  def message(schema: Type): MessageType = {
    val builder = Types.buildMessage()
    schema.asGroupType().getFields.asScala.foreach(builder.addField)
    builder.named(schema.getName)
  }

  // Check if writer schema encodes arrays as a single repeated field inside of an optional or required group
  private[parquet] def hasGroupedArray(writer: Type): Boolean =
    !writer.isPrimitive && writer.asGroupType().getFields.asScala.exists {
      case f if isGroupedArrayType(f) => true
      case f if !f.isPrimitive        => f.asGroupType().getFields.asScala.exists(hasGroupedArray)
      case _                          => false
    }

  private def isGroupedArrayType(f: Type): Boolean =
    !f.isPrimitive &&
      f.getLogicalTypeAnnotation == LogicalTypeAnnotation.listType() && {
        val fields = f.asGroupType().getFields.asScala
        fields.size == 1 && fields.head.isRepetition(Repetition.REPEATED)
      }

  def checkCompatibility(writer: Type, reader: Type): Unit = {
    def listFields(gt: GroupType) =
      s"[${gt.getFields.asScala.map(f => s"${f.getName}: ${f.getRepetition}").mkString(",")}]"

    def isRepetitionBackwardCompatible(w: Type, r: Type) =
      (w.getRepetition, r.getRepetition) match {
        case (Repetition.REQUIRED, Repetition.OPTIONAL) => true
        case (r1, r2)                                   => r1 == r2
      }

    if (
      !isRepetitionBackwardCompatible(writer, reader) ||
      writer.isPrimitive != reader.isPrimitive
    ) {
      throw new InvalidRecordException(
        s"Writer schema `$writer` incompatible with reader schema `$reader``"
      )
    }

    writer match {
      case wg: GroupType =>
        val rg = reader.asGroupType()
        rg.getFields.asScala.foreach { rf =>
          if (wg.containsField(rf.getName)) {
            val wf = wg.getType(rf.getName)
            checkCompatibility(wf, rf)
          } else {
            (
              rf.getLogicalTypeAnnotation != LogicalTypeAnnotation.listType(),
              rf.getRepetition
            ) match {
              case (true, Repetition.REQUIRED) =>
                throw new InvalidRecordException(
                  s"Requested field `${rf.getName}: ${rf.getRepetition}` is not present in written file schema. " +
                    s"Available fields are: ${listFields(wg)}"
                )
              case (true, Repetition.OPTIONAL) =>
                logger.warn(
                  s"Requested field `${rf.getName}: ${rf.getRepetition}` is not present in written file schema " +
                    s"and will be evaluated as `Option.empty`. Available fields are: ${listFields(wg)}"
                )
              case _ =>
            }
          }
        }
      case _: PrimitiveType =>
        val wf = writer.asPrimitiveType()
        val rf = reader.asPrimitiveType()
        if (wf.getPrimitiveTypeName != rf.getPrimitiveTypeName) {
          throw new InvalidRecordException(
            s"Requested ${reader.getName} with primitive type $rf not " +
              s"found; written file schema had type $wf"
          )
        }
      case _ =>
        throw new Exception(s"Unsupported type for $writer")
    }
  }
}
