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

package magnolify.cats

import cats.kernel.CommutativeSemigroup
import magnolia1._

import scala.annotation.implicitNotFound
import scala.collection.compat._

object CommutativeSemigroupDerivation {
  type Typeclass[T] = CommutativeSemigroup[T]

  def join[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = {
    val combineImpl = SemigroupMethods.combine(caseClass)
    val combineNImpl = SemigroupMethods.combineN(caseClass)
    val combineAllOptionImpl = SemigroupMethods.combineAllOption(caseClass)

    new CommutativeSemigroup[T] {
      override def combine(x: T, y: T): T = combineImpl(x, y)
      override def combineN(a: T, n: Int): T = combineNImpl(a, n)
      override def combineAllOption(as: IterableOnce[T]): Option[T] = combineAllOptionImpl(as)
    }
  }

  @implicitNotFound("Cannot derive CommutativeSemigroup for sealed trait")
  private sealed trait Dispatchable[T]
  def split[T: Dispatchable](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = ???

  implicit def apply[T]: Typeclass[T] = macro Magnolia.gen[T]
}
