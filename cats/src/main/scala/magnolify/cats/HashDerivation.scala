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

import cats.Hash
import magnolia1._
import magnolify.shims.MurmurHash3Compat

import scala.util.hashing.MurmurHash3

object HashDerivation {
  type Typeclass[T] = Hash[T]

  def join[T](caseClass: ReadOnlyCaseClass[Typeclass, T]): Typeclass[T] = {
    val eqvImpl = EqMethods.join(caseClass)

    new Hash[T] {
      override def hash(x: T): Int =
        if (caseClass.parameters.isEmpty) {
          caseClass.typeName.short.hashCode
        } else {
          val seed = MurmurHash3Compat.seed(caseClass.typeName.short.hashCode)
          val h = caseClass.parameters.foldLeft(seed) { (h, p) =>
            MurmurHash3.mix(h, p.typeclass.hash(p.dereference(x)))
          }
          MurmurHash3.finalizeHash(h, caseClass.parameters.size)
        }

      override def eqv(x: T, y: T): Boolean = eqvImpl(x, y)
    }
  }

  def split[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = {
    val eqvImpl = EqMethods.split(sealedTrait)

    new Hash[T] {
      override def hash(x: T): Int = sealedTrait.split(x) { sub =>
        sub.typeclass.hash(sub.cast(x))
      }

      override def eqv(x: T, y: T): Boolean = eqvImpl(x, y)
    }
  }

  implicit def apply[T]: Typeclass[T] = macro Magnolia.gen[T]
}
