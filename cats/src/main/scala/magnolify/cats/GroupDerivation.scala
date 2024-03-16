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

import cats.Group
import magnolia1._

import scala.annotation.implicitNotFound
import scala.collection.compat._

object GroupDerivation {
  type Typeclass[T] = Group[T]

  def join[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = {
    val emptyImpl = MonoidMethods.empty(caseClass)
    val combineImpl = SemigroupMethods.combine(caseClass)
    val combineNImpl = GroupMethods.combineN(caseClass)
    val combineAllImpl = MonoidMethods.combineAll(caseClass)
    val combineAllOptionImpl = SemigroupMethods.combineAllOption(caseClass)
    val inverseImpl = GroupMethods.inverse(caseClass)
    val removeImpl = GroupMethods.remove(caseClass)

    new Group[T] {
      override def empty: T = emptyImpl()
      override def combine(x: T, y: T): T = combineImpl(x, y)
      override def combineN(a: T, n: Int): T = combineNImpl(a, n)
      override def combineAll(as: IterableOnce[T]): T = combineAllImpl(as)
      override def combineAllOption(as: IterableOnce[T]): Option[T] = combineAllOptionImpl(as)
      override def inverse(a: T): T = inverseImpl(a)
      override def remove(a: T, b: T): T = removeImpl(a, b)
    }
  }

  @implicitNotFound("Cannot derive Group for sealed trait")
  private sealed trait Dispatchable[T]
  def split[T: Dispatchable](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = ???

  implicit def apply[T]: Typeclass[T] = macro Magnolia.gen[T]
}

private object GroupMethods {
  def combineN[T, Typeclass[T] <: Group[T]](caseClass: CaseClass[Typeclass, T]): (T, Int) => T = {
    val emptyImpl = MonoidMethods.empty(caseClass)
    val combineImpl = SemigroupMethods.combine(caseClass)
    val f = SemigroupMethods.combineNBase(caseClass)
    val inverseImpl = inverse(caseClass)
    (a: T, n: Int) =>
      if (n > 0) {
        f(a, n)
      } else if (n == 0) {
        emptyImpl()
      } else if (n == Int.MinValue) {
        f(inverseImpl(combineImpl(a, a)), 1073741824)
      } else {
        f(inverseImpl(a), -n)
      }
  }

  def inverse[T, Typeclass[T] <: Group[T]](caseClass: CaseClass[Typeclass, T]): T => T =
    a => caseClass.construct(p => p.typeclass.inverse(p.dereference(a)))

  def remove[T, Typeclass[T] <: Group[T]](caseClass: CaseClass[Typeclass, T]): (T, T) => T =
    (a, b) => caseClass.construct(p => p.typeclass.remove(p.dereference(a), p.dereference(b)))
}
