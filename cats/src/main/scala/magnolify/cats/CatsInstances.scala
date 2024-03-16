/*
 * Copyright 2023 Spotify AB
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

package magnolify.cats

trait CatsInstances {
  val BandDerivation: magnolify.cats.BandDerivation.type = magnolify.cats.BandDerivation
  val CommutativeGroupDerivation: magnolify.cats.CommutativeGroupDerivation.type =
    magnolify.cats.CommutativeGroupDerivation
  val CommutativeMonoidDerivation: magnolify.cats.CommutativeMonoidDerivation.type =
    magnolify.cats.CommutativeMonoidDerivation
  val CommutativeSemigroupDerivation: magnolify.cats.CommutativeSemigroupDerivation.type =
    magnolify.cats.CommutativeSemigroupDerivation
  val EqDerivation: magnolify.cats.EqDerivation.type = magnolify.cats.EqDerivation
  val GroupDerivation: magnolify.cats.GroupDerivation.type = magnolify.cats.GroupDerivation
  val HashDerivation: magnolify.cats.HashDerivation.type = magnolify.cats.HashDerivation
  val MonoidDerivation: magnolify.cats.MonoidDerivation.type = magnolify.cats.MonoidDerivation
  val SemigroupDerivation: magnolify.cats.SemigroupDerivation.type =
    magnolify.cats.SemigroupDerivation
  val ShowDerivation: magnolify.cats.ShowDerivation.type = magnolify.cats.ShowDerivation
}
