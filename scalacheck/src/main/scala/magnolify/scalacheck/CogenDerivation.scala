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

package magnolify.scalacheck

import magnolia1._
import org.scalacheck.Cogen

object CogenDerivation {
  type Typeclass[T] = Cogen[T]

  def join[T](caseClass: ReadOnlyCaseClass[Typeclass, T]): Typeclass[T] = Cogen { (seed, t) =>
    caseClass.parameters.foldLeft(seed) { (seed, p) =>
      // inject index to distinguish cases like `(Some(false), None)` and `(None, Some(0))`
      val s = Cogen.cogenInt.perturb(seed, p.index)
      p.typeclass.perturb(s, p.dereference(t))
    }
  }

  def split[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = Cogen { (seed, t: T) =>
    sealedTrait.split(t) { sub =>
      // inject index to distinguish case objects instances
      val s = Cogen.cogenInt.perturb(seed, sub.index)
      sub.typeclass.perturb(s, sub.cast(t))
    }
  }

  implicit def apply[T]: Typeclass[T] = macro Magnolia.gen[T]
}
