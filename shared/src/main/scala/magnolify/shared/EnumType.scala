/*
 * Copyright 2020 Spotify AB
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

package magnolify.shared

import magnolia1._

import scala.reflect.ClassTag
import scala.reflect.macros._
import scala.annotation.{implicitNotFound, nowarn}

sealed trait EnumType[T] extends Serializable { self =>
  def name: String
  def namespace: String
  def values: List[String]
  def valueSet: Set[String]
  def annotations: List[Any]
  def from(v: String): T
  def to(v: T): String

  def map(cm: CaseMapper): EnumType[T] = {
    val cMap = values.map(v => cm.map(v) -> v).toMap
    EnumType.create(
      name,
      namespace,
      values.map(cm.map),
      annotations,
      v => self.from(cMap(v))
    )
  }
}

object EnumType {
  def apply[T](implicit et: EnumType[T]): EnumType[T] = et
  def apply[T](cm: CaseMapper)(implicit et: EnumType[T]): EnumType[T] = et.map(cm)

  def create[T](
    _name: String,
    _namespace: String,
    _values: List[String],
    _annotations: List[Any],
    f: String => T
  ): EnumType[T] = new EnumType[T] {

    @transient private lazy val fMap =
      _values.map(v => v -> f(v)).toMap

    @transient private lazy val gMap =
      _values.map(v => f(v) -> v).toMap

    override val name: String = _name
    override val namespace: String = _namespace
    override val values: List[String] = _values
    override val valueSet: Set[String] = _values.toSet
    override val annotations: List[Any] = _annotations
    override def from(v: String): T = fMap(v)
    override def to(v: T): String = gMap(v)
  }

  // ////////////////////////////////////////////////

  // Java `enum`
  implicit def javaEnumType[T <: Enum[T]](implicit ct: ClassTag[T]): EnumType[T] = {
    val cls: Class[_] = ct.runtimeClass
    val n = ReflectionUtils.name[T]
    val ns = ReflectionUtils.namespace[T]
    val map: Map[String, T] = cls
      .getMethod("values")
      .invoke(null)
      .asInstanceOf[Array[T]]
      .iterator
      .map(v => v.name() -> v)
      .toMap
    EnumType.create(n, ns, map.keys.toList, cls.getAnnotations.toList, map(_))
  }

  // ////////////////////////////////////////////////

  // Scala `Enumeration`
  implicit def scalaEnumType[T <: Enumeration#Value: AnnotationType]: EnumType[T] =
    macro scalaEnumTypeImpl[T]

  def scalaEnumTypeImpl[T: c.WeakTypeTag](
    c: whitebox.Context
  )(annotations: c.Expr[AnnotationType[T]]): c.Tree = {
    import c.universe._
    val wtt = weakTypeTag[T]
    val ref = wtt.tpe.asInstanceOf[TypeRef]
    val fn = ref.pre.typeSymbol.asClass.fullName
    val idx = fn.lastIndexOf('.')
    val n = fn.drop(idx + 1) // `object <Namespace> extends Enumeration`
    val ns = fn.take(idx)
    val list = q"${ref.pre.termSymbol}.values.toList.sortBy(_.id).map(_.toString)"
    val map = q"${ref.pre.termSymbol}.values.map(x => x.toString -> x).toMap"

    q"""
        _root_.magnolify.shared.EnumType.create[$wtt](
          $n, $ns, $list, $annotations.annotations, $map.apply(_))
     """
  }

  // ////////////////////////////////////////////////

  // Scala ADT
  def adtEnumType[T]: EnumType[T] = macro Magnolia.gen[T]

  implicit def gen[T]: EnumType[T] = macro Magnolia.gen[T]

  type Typeclass[T] = EnumType[T]

  // EnumType can only be split into objects with fixed name
  // Avoid invalid ADT derivation involving products by requiring
  // implicit EnumValue type-class in magnolia join
  @implicitNotFound("Cannot derive EnumType.EnumValue. EnumType only works for sum types")
  trait EnumValue[T]
  implicit def genEnumValue[T]: EnumValue[T] = macro genEnumValueMacro[T]
  def genEnumValueMacro[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val symbol = tpe.typeSymbol
    if (symbol.isModuleClass) {
      q"new _root_.magnolify.shared.EnumType.EnumValue[$tpe]{}"
    } else {
      c.abort(c.enclosingPosition, "EnumType value must be an object")
    }
  }

  @nowarn
  def join[T: EnumValue](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = {
    val n = caseClass.typeName.short
    val ns = caseClass.typeName.owner
    EnumType.create(
      n,
      ns,
      List(n),
      caseClass.annotations.toList,
      _ => caseClass.rawConstruct(Nil)
    )
  }

  def split[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = {
    val n = sealedTrait.typeName.short
    val ns = sealedTrait.typeName.owner
    val subs = sealedTrait.subtypes.map(_.typeclass)
    val values = subs.flatMap(_.values).toList
    val annotations = (sealedTrait.annotations ++ subs.flatMap(_.annotations)).toList
    EnumType.create(
      n,
      ns,
      values,
      annotations,
      // it is ok to use the inefficient find here because it will be called only once
      // and cached inside an instance of EnumType
      v => subs.find(_.name == v).get.from(v)
    )
  }
}
