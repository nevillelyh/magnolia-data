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

import cats.Monoid
import magnolia1._

import scala.annotation.implicitNotFound
import scala.collection.compat.immutable.ArraySeq
import scala.collection.compat._

object MonoidDerivation {
  type Typeclass[T] = Monoid[T]

  def join[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = {
    val emptyImpl = MonoidMethods.empty(caseClass)
    val combineImpl = SemigroupMethods.combine(caseClass)
    val combineNImpl = MonoidMethods.combineN(caseClass)
    val combineAllImpl = MonoidMethods.combineAll(caseClass)
    val combineAllOptionImpl = SemigroupMethods.combineAllOption(caseClass)

    new Monoid[T] {
      override def empty: T = emptyImpl()
      override def combine(x: T, y: T): T = combineImpl(x, y)
      override def combineN(a: T, n: Int): T = combineNImpl(a, n)
      override def combineAll(as: IterableOnce[T]): T = combineAllImpl(as)
      override def combineAllOption(as: IterableOnce[T]): Option[T] = combineAllOptionImpl(as)
    }
  }

  @implicitNotFound("Cannot derive Monoid for sealed trait")
  private sealed trait Dispatchable[T]
  def split[T: Dispatchable](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = ???

  implicit def apply[T]: Typeclass[T] = macro Magnolia.gen[T]
}

private object MonoidMethods {
  def empty[T, Typeclass[T] <: Monoid[T]](caseClass: CaseClass[Typeclass, T]): () => T =
    new Function0[T] with Serializable {
      @transient private lazy val value = caseClass.construct(_.typeclass.empty)
      override def apply(): T = value
    }

  def combineN[T, Typeclass[T] <: Monoid[T]](caseClass: CaseClass[Typeclass, T]): (T, Int) => T = {
    val emptyImpl = empty(caseClass)
    val f = SemigroupMethods.combineNBase(caseClass)
    (a: T, n: Int) =>
      if (n < 0) {
        throw new IllegalArgumentException("Repeated combining for monoids must have n >= 0")
      } else if (n == 0) {
        emptyImpl()
      } else {
        f(a, n)
      }
  }

  def combineAll[T, Typeclass[T] <: Monoid[T]](
    caseClass: CaseClass[Typeclass, T]
  ): IterableOnce[T] => T = {
    val combineImpl = SemigroupMethods.combine(caseClass)
    val emptyImpl = MonoidMethods.empty(caseClass)

    {
      case it: Iterable[T] if it.nonEmpty =>
        // input is re-iterable and non-empty, combineAll on each field
        val result = Array.fill[Any](caseClass.parameters.length)(null)
        var i = 0
        while (i < caseClass.parameters.length) {
          val p = caseClass.parameters(i)
          result(i) = p.typeclass.combineAll(it.iterator.map(p.dereference))
          i += 1
        }
        caseClass.rawConstruct(ArraySeq.unsafeWrapArray(result))
      case xs => xs.iterator.foldLeft(emptyImpl())(combineImpl)
    }
  }
}
