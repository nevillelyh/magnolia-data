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

package magnolify.bigquery

import magnolify.shared._

package object unsafe {
  implicit val trfByte = TableRowField.from[Long](_.toByte)(_.toLong)
  implicit val trfChar = TableRowField.from[Long](_.toChar)(_.toLong)
  implicit val trfShort = TableRowField.from[Long](_.toShort)(_.toLong)
  implicit val trfInt = TableRowField.from[Long](_.toInt)(_.toLong)
  implicit val trfFloat = TableRowField.from[Double](_.toFloat)(_.toDouble)

  implicit def trfEnum[T](implicit et: EnumType[T], lp: shapeless.LowPriority): TableRowField[T] =
    TableRowField.from[String](et.from)(et.to)

  implicit def trfUnsafeEnum[T](implicit
    et: EnumType[T],
    lp: shapeless.LowPriority
  ): TableRowField[UnsafeEnum[T]] =
    TableRowField.from[String](UnsafeEnum.from(_))(UnsafeEnum.to(_))
}
