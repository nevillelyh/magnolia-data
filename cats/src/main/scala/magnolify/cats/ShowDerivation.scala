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

import cats.Show
import magnolia1._

object ShowDerivation {
  type Typeclass[T] = Show[T]

  def join[T](caseClass: ReadOnlyCaseClass[Typeclass, T]): Typeclass[T] = new Show[T] {
    override def show(x: T): String = caseClass.parameters
      .map(p => s"${p.label} = ${p.typeclass.show(p.dereference(x))}")
      .mkString(s"${caseClass.typeName.full} {", ", ", "}")
  }

  def split[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = new Show[T] {
    override def show(x: T): String =
      sealedTrait.split(x)(sub => sub.typeclass.show(sub.cast(x)))
  }

  implicit def apply[T]: Typeclass[T] = macro Magnolia.gen[T]
}
