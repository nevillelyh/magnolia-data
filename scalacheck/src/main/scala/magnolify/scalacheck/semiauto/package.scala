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

package magnolify.scalacheck

import org.scalacheck.{Arbitrary, Cogen}

package object semiauto {

  @deprecated("Use Arbitrary.gen[T] instead", "0.7.0")
  val ArbitraryDerivation = magnolify.scalacheck.ArbitraryDerivation
  @deprecated("Use Gogen.gen[T] instead", "0.7.0")
  val CogenDerivation = magnolify.scalacheck.CogenDerivation

  implicit def genArbitrary(a: Arbitrary.type): magnolify.scalacheck.ArbitraryDerivation.type =
    magnolify.scalacheck.ArbitraryDerivation
  implicit def genCogen(c: Cogen.type): magnolify.scalacheck.CogenDerivation.type =
    magnolify.scalacheck.CogenDerivation
}
