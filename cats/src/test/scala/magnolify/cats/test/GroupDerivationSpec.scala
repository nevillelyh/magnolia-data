/*
 * Copyright 2019 Spotify AB.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package magnolify.cats.test

import cats._
import cats.instances.all._
import cats.kernel.CommutativeGroup
import cats.kernel.laws.discipline._
import magnolify.cats.auto._
import magnolify.scalacheck.auto._
import magnolify.test.Simple._
import magnolify.test._
import org.scalacheck._

import scala.reflect._

object GroupDerivationSpec extends MagnolifySpec("GroupDerivation") {
  private def test[T: Arbitrary: ClassTag: Eq: Group]: Unit = {
    ensureSerializable(implicitly[Group[T]])
    include(GroupTests[T].group.all, className[T] + ".")
  }

  test[Integers]
}

object CommutativeGroupDerivationSpec extends MagnolifySpec("CommutativeGroupDerivation") {
  private def test[T: Arbitrary: ClassTag: Eq: CommutativeGroup]: Unit = {
    ensureSerializable(implicitly[CommutativeGroup[T]])
    include(CommutativeGroupTests[T].commutativeGroup.all, className[T] + ".")
  }

  test[Integers]
}
