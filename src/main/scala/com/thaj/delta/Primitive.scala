package com.thaj.delta

import cats.Eq
import cats.evidence.As
import shapeless.ops.hlist.IsHCons
import shapeless.{Generic, HList, HNil}
import cats.syntax.eq._

trait Primitive[A] {
  def isAnyVal: A As AnyVal
  def eqv: Eq[A]
}

object Primitive {
  def apply[A](implicit ev: Primitive[A]): Primitive[A] = ev

  implicit def getAnyValWrapperWithoutEq[A, T <: HList, H](
    implicit I: A As AnyVal,
    G: Generic.Aux[A, T],
    IsH : IsHCons.Aux[T, H, HNil],
    E: Eq[H]
  ): Primitive[A] =
    new Primitive[A] {
      override def isAnyVal: A As AnyVal = I
      override def eqv: Eq[A] =
        (x, y) => G.to(x).head === G.to(y).head
    }


  /**
    * FindDeltaMeta behaviors can be overriden as shown:
    *
    * case class Yaml(x: String) extends AnyVal
    * case class Outer(yaml: Yaml)
    *
    * val outer1 = Outer(Yaml("input"))
    * val outer2 = Outer(Yaml("inputjunk"))
    *
    * {{{ FindDeltaMeta[Outer].apply(outer1, outer2) }}} returns Meta(key = yaml, previousValue = input, newValue = inputJunk)
    *
    * However, if we override the primitive of yaml:
    *
    * implicit val primitiveYaml: Primitive[Yaml] =
    *   Primitive.fromEquality( (a, b) => a.replace("junk", "") === b.replace("junk", "")_)
    *
    * {{{ FindDeltaMeta[Outer].apply(outer1, outer2) }}} returns Meta.empty.
    */
  def fromEquality[A](equality: (A, A) => Boolean)(implicit ev: A As AnyVal): Primitive[A] =
    new Primitive[A] {
      override def isAnyVal: As[A, AnyVal] = ev
      override def eqv: Eq[A] = equality(_, _)
    }
}