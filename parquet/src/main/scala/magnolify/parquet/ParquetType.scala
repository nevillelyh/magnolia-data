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

import magnolify.shared.{Converter => _, _}
import org.apache.avro.{Schema => AvroSchema}
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.mapreduce.Job
import org.apache.parquet.avro.AvroSchemaConverter
import org.apache.parquet.hadoop.{
  api => hadoop,
  ParquetInputFormat,
  ParquetOutputFormat,
  ParquetReader,
  ParquetWriter
}
import org.apache.parquet.io.api._
import org.apache.parquet.io.{InputFile, OutputFile}
import org.apache.parquet.schema.{MessageType, Type}
import org.slf4j.LoggerFactory
import org.typelevel.scalaccompat.annotation.nowarn

sealed trait ParquetArray

/**
 * Add `import magnolify.parquet.ParquetArray.AvroCompat._` to generate generate Avro-compatible
 * array schemas. This import is DEPRECATED. Instead, pass the following option to your Parquet
 * Configuration:
 *
 * magnolify.parquet.write-grouped-arrays: true
 */
object ParquetArray {
  implicit case object default extends ParquetArray

  @deprecated(
    message =
      "AvroCompat import is deprecated; set Parquet Configuration option `magnolify.parquet.write-grouped-arrays: true` instead",
    since = "0.8.0"
  )
  object AvroCompat {
    implicit case object avroCompat extends ParquetArray
  }
}

sealed trait ParquetType[T] extends Serializable {
  import ParquetType._

  def schema: MessageType
  def avroSchema: AvroSchema

  def setupInput(job: Job): Unit = {
    job.setInputFormatClass(classOf[ParquetInputFormat[T]])
    ParquetInputFormat.setReadSupportClass(job, classOf[ReadSupport[T]])
    job.getConfiguration.set(ReadTypeKey, SerializationUtils.toBase64(this))
  }

  def setupOutput(job: Job): Unit = {
    job.setOutputFormatClass(classOf[ParquetOutputFormat[T]])
    ParquetOutputFormat.setWriteSupportClass(job, classOf[WriteSupport[T]])
    job.getConfiguration.set(WriteTypeKey, SerializationUtils.toBase64(this))
  }

  def readSupport: ReadSupport[T] = new ReadSupport[T](this)
  def writeSupport: WriteSupport[T] = new WriteSupport[T](this)

  def readBuilder(file: InputFile): ReadBuilder[T] = new ReadBuilder(file, readSupport)
  def writeBuilder(file: OutputFile): WriteBuilder[T] = new WriteBuilder(file, writeSupport)

  private[parquet] def properties: MagnolifyParquetProperties

  private[parquet] def write(c: RecordConsumer, v: T): Unit = ()
  private[parquet] def newConverter(writerSchema: Type): TypeConverter[T] = null
}

object ParquetType {
  private val logger = LoggerFactory.getLogger(this.getClass)

  implicit def apply[T](implicit f: ParquetField[T], pa: ParquetArray): ParquetType[T] =
    ParquetType(CaseMapper.identity, MagnolifyParquetProperties.Default)

  def apply[T](
    cm: CaseMapper
  )(implicit f: ParquetField[T], pa: ParquetArray): ParquetType[T] =
    ParquetType[T](cm, MagnolifyParquetProperties.Default)(f, pa)

  def apply[T](
    conf: Configuration
  )(implicit f: ParquetField[T], pa: ParquetArray): ParquetType[T] =
    ParquetType[T](
      CaseMapper.identity, {
        val writeGroupedArraysOpt =
          Option(conf.get(MagnolifyParquetConfigurationCompat.WriteGroupedArrays)).map(_.toBoolean)
        val writeMetadataOpt = Option(
          conf.get(MagnolifyParquetConfigurationCompat.WriteAvroSchemaToMetadata)
        ).map(_.toBoolean)

        new MagnolifyParquetProperties {
          override def writeGroupedArrays: Boolean =
            writeGroupedArraysOpt.getOrElse(super.writeGroupedArrays)
          override def writeAvroSchemaToMetadata: Boolean =
            writeMetadataOpt.getOrElse(super.writeAvroSchemaToMetadata)
        }
      }
    )(f, pa)

  def apply[T](
    properties: MagnolifyParquetProperties
  )(implicit f: ParquetField[T], pa: ParquetArray): ParquetType[T] =
    ParquetType[T](CaseMapper.identity, properties)(f, pa)

  def apply[T](
    cm: CaseMapper,
    props: MagnolifyParquetProperties
  )(implicit f: ParquetField[T], pa: ParquetArray): ParquetType[T] = f match {
    case r: ParquetField.Record[_] =>
      new ParquetType[T] {
        @transient override def schema: MessageType =
          Schema.message(r.schema(cm, properties))

        @transient override def avroSchema: AvroSchema = {
          val s = new AvroSchemaConverter().convert(schema)
          // add doc to avro schema
          val fieldDocs = f.fieldDocs(cm)
          SchemaUtil.deepCopy(s, f.typeDoc, fieldDocs.get)
        }

        override def write(c: RecordConsumer, v: T): Unit =
          r.write(c, v)(cm, properties)
        override private[parquet] def newConverter(writerSchema: Type): TypeConverter[T] =
          r.newConverter(writerSchema)

        override private[parquet] def properties: MagnolifyParquetProperties = props
      }
    case _ =>
      throw new IllegalArgumentException(s"ParquetType can only be created from Record. Got $f")
  }

  val ReadTypeKey = "parquet.type.read.type"
  val WriteTypeKey = "parquet.type.write.type"

  class ReadBuilder[T](file: InputFile, val readSupport: ReadSupport[T])
      extends ParquetReader.Builder[T](file) {
    override def getReadSupport: ReadSupport[T] = readSupport
  }

  class WriteBuilder[T](file: OutputFile, val writeSupport: WriteSupport[T])
      extends ParquetWriter.Builder[T, WriteBuilder[T]](file) {
    override def self(): WriteBuilder[T] = this
    override def getWriteSupport(conf: Configuration): WriteSupport[T] = writeSupport
  }

  // From AvroReadSupport
  private val AVRO_SCHEMA_METADATA_KEY = "parquet.avro.schema"

  class ReadSupport[T](private var parquetType: ParquetType[T]) extends hadoop.ReadSupport[T] {
    def this() = this(null)

    override def init(context: hadoop.InitContext): hadoop.ReadSupport.ReadContext = {
      if (parquetType == null) {
        // Use deprecated getConfiguration
        // Recommended getParquetConfiguration is only available for parquet 1.14+
        val readKeyType = context.getConfiguration.get(ReadTypeKey): @nowarn("cat=deprecation")
        parquetType = SerializationUtils.fromBase64[ParquetType[T]](readKeyType)
      }

      val requestedSchema = {
        val s = Schema.message(parquetType.schema)
        // If reading Avro, roundtrip schema using parquet-avro converter to ensure array compatibility;
        // magnolify-parquet does not automatically wrap repeated fields into a group like parquet-avro does
        if (Schema.hasGroupedArray(context.getFileSchema)) {
          val converter = new AvroSchemaConverter()
          converter.convert(converter.convert(s))
        } else {
          s
        }
      }
      Schema.checkCompatibility(context.getFileSchema, requestedSchema)
      new hadoop.ReadSupport.ReadContext(requestedSchema, java.util.Collections.emptyMap())
    }

    override def prepareForRead(
      configuration: Configuration,
      keyValueMetaData: java.util.Map[String, String],
      fileSchema: MessageType,
      readContext: hadoop.ReadSupport.ReadContext
    ): RecordMaterializer[T] =
      new RecordMaterializer[T] {
        private val root = parquetType.newConverter(fileSchema)
        override def getCurrentRecord: T = root.get
        override def getRootConverter: GroupConverter = root.asGroupConverter()
      }
  }

  class WriteSupport[T](private var parquetType: ParquetType[T]) extends hadoop.WriteSupport[T] {
    def this() = this(null)

    override def getName: String = "magnolify"

    private var recordConsumer: RecordConsumer = null

    override def init(configuration: Configuration): hadoop.WriteSupport.WriteContext = {
      if (parquetType == null) {
        parquetType = SerializationUtils.fromBase64[ParquetType[T]](configuration.get(WriteTypeKey))
      }

      val schema = Schema.message(parquetType.schema)
      val metadata = new java.util.HashMap[String, String]()

      if (parquetType.properties.writeAvroSchemaToMetadata) {
        try {
          metadata.put(
            AVRO_SCHEMA_METADATA_KEY,
            parquetType.avroSchema.toString()
          )
        } catch {
          // parquet-avro has greater schema restrictions than magnolify-parquet, e.g., parquet-avro does not
          // support Maps with non-Binary key types
          case e: IllegalArgumentException =>
            logger.warn(
              s"Writer schema `$schema` contains a type not supported by Avro schemas; will not write " +
                s"key $AVRO_SCHEMA_METADATA_KEY to file metadata",
              e
            )
        }
      }

      new hadoop.WriteSupport.WriteContext(schema, metadata)
    }

    override def prepareForWrite(recordConsumer: RecordConsumer): Unit =
      this.recordConsumer = recordConsumer

    override def write(record: T): Unit = {
      recordConsumer.startMessage()
      parquetType.write(recordConsumer, record)
      recordConsumer.endMessage()
    }
  }
}
