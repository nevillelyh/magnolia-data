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

package magnolify.cats

import cats.Eq
import magnolia1._

object EqDerivation {
  type Typeclass[T] = Eq[T]

  def join[T](caseClass: ReadOnlyCaseClass[Typeclass, T]): Typeclass[T] =
    Eq.instance(EqMethods.join(caseClass))

  def split[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] =
    Eq.instance(EqMethods.split(sealedTrait))

  implicit def apply[T]: Typeclass[T] = macro Magnolia.gen[T]
}

private object EqMethods {
  def join[T, Typeclass[T] <: Eq[T]](
    caseClass: ReadOnlyCaseClass[Typeclass, T]
  ): (T, T) => Boolean =
    (x, y) => caseClass.parameters.forall(p => p.typeclass.eqv(p.dereference(x), p.dereference(y)))

  def split[T, Typeclass[T] <: Eq[T]](
    sealedTrait: SealedTrait[Typeclass, T]
  ): (T, T) => Boolean =
    (x, y) =>
      sealedTrait.split(x) { sub =>
        sub.cast.isDefinedAt(y) && sub.typeclass.eqv(sub.cast(x), sub.cast(y))
      }
}
