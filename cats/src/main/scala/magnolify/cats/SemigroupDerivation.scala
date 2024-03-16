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

import cats.Semigroup
import magnolia1._

import scala.annotation.implicitNotFound
import scala.collection.compat.immutable.ArraySeq
import scala.collection.compat._

object SemigroupDerivation {
  type Typeclass[T] = Semigroup[T]

  def join[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = {
    val combineImpl = SemigroupMethods.combine(caseClass)
    val combineNImpl = SemigroupMethods.combineN(caseClass)
    val combineAllOptionImpl = SemigroupMethods.combineAllOption(caseClass)

    new Semigroup[T] {
      override def combine(x: T, y: T): T = combineImpl(x, y)
      override def combineN(a: T, n: Int): T = combineNImpl(a, n)
      override def combineAllOption(as: IterableOnce[T]): Option[T] = combineAllOptionImpl(as)
    }
  }

  @implicitNotFound("Cannot derive Semigroup for sealed trait")
  private sealed trait Dispatchable[T]
  def split[T: Dispatchable](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = ???

  implicit def apply[T]: Typeclass[T] = macro Magnolia.gen[T]
}

private object SemigroupMethods {
  def combine[T, Typeclass[T] <: Semigroup[T]](caseClass: CaseClass[Typeclass, T]): (T, T) => T =
    (x, y) => caseClass.construct(p => p.typeclass.combine(p.dereference(x), p.dereference(y)))

  def combineNBase[T, Typeclass[T] <: Semigroup[T]](
    caseClass: CaseClass[Typeclass, T]
  ): (T, Int) => T =
    (a: T, n: Int) => caseClass.construct(p => p.typeclass.combineN(p.dereference(a), n))

  def combineN[T, Typeclass[T] <: Semigroup[T]](
    caseClass: CaseClass[Typeclass, T]
  ): (T, Int) => T = {
    val f = combineNBase(caseClass)
    (a: T, n: Int) =>
      if (n <= 0) {
        throw new IllegalArgumentException("Repeated combining for semigroups must have n > 0")
      } else {
        f(a, n)
      }
  }

  def combineAllOption[T, Typeclass[T] <: Semigroup[T]](
    caseClass: CaseClass[Typeclass, T]
  ): IterableOnce[T] => Option[T] = {
    val combineImpl = combine(caseClass)

    {
      case it: Iterable[T] if it.nonEmpty =>
        // input is re-iterable and non-empty, combineAllOption on each field
        val result = Array.fill[Any](caseClass.parameters.length)(null)
        var i = 0
        while (i < caseClass.parameters.length) {
          val p = caseClass.parameters(i)
          result(i) = p.typeclass.combineAllOption(it.iterator.map(p.dereference)).get
          i += 1
        }
        Some(caseClass.rawConstruct(ArraySeq.unsafeWrapArray(result)))
      case xs =>
        xs.iterator.reduceOption(combineImpl)
    }
  }
}
