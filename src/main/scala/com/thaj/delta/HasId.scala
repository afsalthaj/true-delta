package com.thaj.delta

import cats.evidence.As
import shapeless.ops.hlist.IsHCons
import shapeless.{HList, LabelledGeneric}

trait HasId[A] { self =>
  def id(a: A): String
}

object HasId {
  def apply[A](implicit ev: HasId[A]): HasId[A] = ev

  implicit val intName: HasId[Int] = _.toString
  implicit val stringName: HasId[String] = _.toString
  implicit val doubleName: HasId[Double] = _.toString
  implicit val longName: HasId[Long] = _.toString

  implicit def hListWithSimpleAnyVal[A, K <: Symbol, T <: HList](
    implicit
    IsAnyVal: A As AnyVal,
    L: LabelledGeneric.Aux[A, T],
    E: IsHCons[T]
  ): HasId[A] = a => L.to(a).head.toString
}
