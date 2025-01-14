/*
 * Copyright 2024 Spotify AB
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

import java.util.Objects

trait MagnolifyParquetProperties extends Serializable {
  def writeGroupedArrays: Boolean = false
  def writeAvroSchemaToMetadata: Boolean = true

  private[parquet] final def schemaUniquenessKey: Int = Objects.hash(writeGroupedArrays)
}

object MagnolifyParquetProperties {
  val Default: MagnolifyParquetProperties = new MagnolifyParquetProperties {}
}

/**
 * If set in your core-site.xml or an explicit Configruation object passed to ParquetType, will be
 * parsed into MagnolifyParquetProperties
 */
object MagnolifyParquetConfigurationCompat {
  val WriteGroupedArrays: String = "magnolify.parquet.write-grouped-arrays"
  val WriteAvroSchemaToMetadata: String = "magnolify.parquet.write-avro-schema"
}
