/*
 * Copyright 2023 Spotify AB
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

import org.scalacheck.{Arbitrary, Cogen}

import scala.reflect.macros.*
object ScalaCheckMacros {
  def autoDerivationArbitraryMacro[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._
    val wtt = weakTypeTag[T]
    q"""_root_.magnolify.scalacheck.ArbitraryDerivation.gen[$wtt]"""
  }

  def autoDerivationCogenMacro[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._
    val wtt = weakTypeTag[T]
    q"""_root_.magnolify.scalacheck.CogenDerivation.gen[$wtt]"""
  }

}

trait AutoDerivations {
  implicit def autoDerivationArbitrary[T]: Arbitrary[T] =
    macro ScalaCheckMacros.autoDerivationArbitraryMacro[T]
  implicit def autoDerivationCogen[T]: Cogen[T] =
    macro ScalaCheckMacros.autoDerivationCogenMacro[T]
}
