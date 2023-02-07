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

package magnolify.guava

import com.google.common.hash.Funnel
import magnolify.guava.semiauto.FunnelImplicits

import scala.reflect.macros._

package object auto extends FunnelInstance0 {
  val FunnelDerivation = semiauto.FunnelDerivation
}

object FunnelMacros {
  def genFunnelMacro[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._
    val wtt = weakTypeTag[T]
    q"""_root_.magnolify.guava.semiauto.FunnelDerivation.apply[$wtt]"""
  }
}

trait FunnelInstance0 extends FunnelImplicits with FunnelInstance1

trait FunnelInstance1 extends FunnelInstance2

trait FunnelInstance2 {
  implicit def genFunnel[T]: Funnel[T] = macro FunnelMacros.genFunnelMacro[T]
}
