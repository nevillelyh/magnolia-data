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
import org.apache.parquet.schema.MessageType
import org.slf4j.LoggerFactory

sealed trait ParquetArray

/**
 * Add `import magnolify.parquet.ParquetArray.AvroCompat._` to generate AVRO schema on write
 */
object ParquetArray {
  implicit case object default extends ParquetArray

  object AvroCompat {
    implicit case object avroCompat extends ParquetArray
  }
}

sealed trait ParquetType[T] extends Serializable {
  import ParquetType._

  def schema: MessageType
  def avroSchema: AvroSchema
  def avroCompat: Boolean

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

  def write(c: RecordConsumer, v: T): Unit
  def newConverter(): TypeConverter[T]
}

object ParquetType {
  private val logger = LoggerFactory.getLogger(this.getClass)

  implicit def apply[T](implicit f: ParquetField[T], pa: ParquetArray): ParquetType[T] =
    ParquetType(CaseMapper.identity)

  def apply[T](
    cm: CaseMapper
  )(implicit f: ParquetField[T], pa: ParquetArray): ParquetType[T] = f match {
    case r: ParquetField.Record[_] =>
      new ParquetType[T] {
        @transient override lazy val schema: MessageType = Schema.message(r.schema(cm))
        @transient override lazy val avroSchema: AvroSchema = {
          val s = new AvroSchemaConverter().convert(schema)
          // add doc to avro schema
          val fieldDocs = f.fieldDocs(cm)
          SchemaUtil.deepCopy(s, f.typeDoc, fieldDocs.get)
        }

        override val avroCompat: Boolean =
          pa == ParquetArray.AvroCompat.avroCompat || f.hasAvroArray
        override def write(c: RecordConsumer, v: T): Unit = r.write(c, v)(cm)
        override def newConverter(): TypeConverter[T] = r.newConverter()
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
  private val OLD_AVRO_SCHEMA_METADATA_KEY = "avro.schema"

  class ReadSupport[T](private var parquetType: ParquetType[T]) extends hadoop.ReadSupport[T] {
    def this() = this(null)

    override def init(context: hadoop.InitContext): hadoop.ReadSupport.ReadContext = {
      if (parquetType == null) {
        parquetType =
          SerializationUtils.fromBase64[ParquetType[T]](context.getConfiguration.get(ReadTypeKey))
      }

      val metadata = context.getKeyValueMetadata
      val model = metadata.get(ParquetWriter.OBJECT_MODEL_NAME_PROP)
      val isAvroFile = (model != null && model.contains("avro")) ||
        metadata.containsKey(AVRO_SCHEMA_METADATA_KEY) ||
        metadata.containsKey(OLD_AVRO_SCHEMA_METADATA_KEY)
      if (isAvroFile && !parquetType.avroCompat) {
        logger.warn(
          "Parquet file was written from Avro records, " +
            "`import magnolify.parquet.ParquetArray.AvroCompat._` to read correctly"
        )
      }
      if (!isAvroFile && parquetType.avroCompat) {
        logger.warn(
          "Parquet file was not written from Avro records, " +
            "remove `import magnolify.parquet.ParquetArray.AvroCompat._` to read correctly"
        )
      }

      val requestedSchema = Schema.message(parquetType.schema)
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
        private val root = parquetType.newConverter
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
      if (parquetType.avroCompat) {
        // This overrides `WriteSupport#getName`
        metadata.put(ParquetWriter.OBJECT_MODEL_NAME_PROP, "avro")
        metadata.put(AVRO_SCHEMA_METADATA_KEY, parquetType.avroSchema.toString())
      } else {
        logger.warn(
          "Parquet file is being written with no avro compatibility, this mode is not " +
            "producing schema. Add `import magnolify.parquet.ParquetArray.AvroCompat._` to " +
            "generate schema"
        )
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
