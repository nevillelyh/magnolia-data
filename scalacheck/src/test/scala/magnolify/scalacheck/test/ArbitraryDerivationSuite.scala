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

package magnolify.scalacheck.test

import magnolify.scalacheck.auto._
import magnolify.shims._
import magnolify.test.ADT._
import magnolify.test.Simple._
import magnolify.test._
import org.scalacheck._
import org.scalacheck.rng.Seed

import scala.reflect._

class ArbitraryDerivationSuite extends MagnolifySuite {
  private def test[T: Arbitrary: ClassTag]: Unit = test[T](null)
  private def test[T: Arbitrary: ClassTag](suffix: String): Unit = test[T, T](identity, suffix)

  private def test[T: ClassTag, U](f: T => U, suffix: String = null)(implicit
    t: Arbitrary[T]
  ): Unit = {
    val g = ensureSerializable(t).arbitrary
    val name = className[T] + (if (suffix == null) "" else "." + suffix)
    val prms = Gen.Parameters.default
    // `forAll(Gen.listOfN(10, g))` fails for `Repeated` & `Collections` when size parameter <= 1
    property(s"$name.uniqueness") {
      Prop.forAll { seed: Seed =>
        val xs = Gen.listOfN(10, g)(prms, seed).get
        xs.iterator.map(f).toSet.size > 1
      }
    }
    property(s"$name.consistency") {
      Prop.forAll { l: Long =>
        val seed = Seed(l) // prevent Magnolia from deriving `Seed`
        f(g(prms, seed).get) == f(g(prms, seed).get)
      }
    }
  }

  test[Numbers]
  test[Required]
  test[Nullable]

  {
    import Collections._
    import MoreCollectionsBuildable._ // scala 2.12 does not find implicit Buildable for some collections
    test[Repeated]
    test((c: Collections) => (c.a.toList, c.l, c.v))
    test[MoreCollections]
  }

  test[Nested]

  {
    implicit val arbInt: Arbitrary[Int] = Arbitrary(Gen.chooseNum(0, 100))
    implicit val arbLong: Arbitrary[Long] = Arbitrary(Gen.chooseNum(100, 10000))
    property("implicits") {
      Prop.forAll { x: Integers =>
        x.i >= 0 && x.i <= 100 && x.l >= 100 && x.l <= 10000
      }
    }
  }

  {
    import Custom._
    test[Custom]
  }

  {
    import magnolify.scalacheck.semiauto.ArbitraryDerivation.Fallback
    implicit val f: Fallback[Node] = Fallback[Leaf]
    test[Node]
  }

  {
    import magnolify.scalacheck.semiauto.ArbitraryDerivation.Fallback
    implicit val f: Fallback[GNode[Int]] = Fallback(Gen.const(GLeaf(0)))
    test[GNode[Int]]("Fallback(G: Gen[T])")
  }

  {
    import magnolify.scalacheck.semiauto.ArbitraryDerivation.Fallback
    implicit val f: Fallback[GNode[Int]] = Fallback(GLeaf(0))
    test[GNode[Int]]("Fallback(v: T)")
  }

  {
    import magnolify.scalacheck.semiauto.ArbitraryDerivation.Fallback
    implicit val f: Fallback[GNode[Int]] = Fallback[GLeaf[Int]]
    test[GNode[Int]]("Fallback[T]")
  }

  test[Shape]
  test[Color]

  property("Seed") {
    Prop.forAll { seed: Seed =>
      seed.next != seed
    }
  }
}
