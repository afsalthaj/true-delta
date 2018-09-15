package com.thaj.delta

import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}
import shapeless.labelled.FieldType
import cats.evidence.As
import cats.Eq
import cats.data.Ior
import cats.syntax.eq._
import cats.instances.string._
import enumeratum.EnumEntry
import DeltaMeta.Meta

import scala.annotation.implicitNotFound

/**
  * Recursively looks through data structures of any complexity
  * and tracks down the changes as a Meta returning along with recursively appended key structure.
  *
  * To override comparison behavior of primitives:
  *
  * {{{
  *   import cats.syntax.eq._
  *   import cats.instance.string._
  *
  *   final case class Yaml(s: String) extends AnyVal
  *   implicit val primitiveYaml: Primitive[Yaml] =
  *     Primitive.fromEquality( (a, b) => a.replace("junk", "") === b.replace("junk", ""))
  * }}}
  *
  * Intentional with toString and asInstanceOf to increase compile time speed.
  * They are safe as it bounded by cats evidences.
  */
@implicitNotFound(
  msg = "HINT: Ensure (1) cats.implicits._ is in scope.\n" +
    "(2) If the members/fields of {A} itself is a product (case class), FindDeltaMeta expects it to have HasName instance.\n" +
    "This is required because the changes/updates in a member of {A} is tracked only if their names (or ids) are same.\n" +
    "Ex: case class Outer(inner: Inner), such that inner is a product, then, to have FindDeltaMeta[Outer], " +
    "there should be a HasName instance for inner." +
    "(3) If the members/fields of {A} are AnyVals or primitives then it requires cats.Eq instance."
)
trait FindDeltaMeta[A] {
  def apply(a: A, b: A): Meta
}

object FindDeltaMeta extends LowPriorityInstances0 {
  def apply[A](implicit ev: FindDeltaMeta[A]): FindDeltaMeta[A] = ev
}

trait LowPriorityInstances0 extends LowPriorityInstances1 {
  implicit def hNilFindDeltaMeta: FindDeltaMeta[HNil] =
    (_, _) => Meta.empty

  implicit def findDiffA[A, R <: HList](
    implicit E: LabelledGeneric.Aux[A, R],
    D: FindDeltaMeta[R]
  ): FindDeltaMeta[A] = {
    (a, b) => D.apply(E.to(a), E.to(b))
  }
}

trait LowPriorityInstances1 extends LowPriorityInstances2 {
  implicit def hListWithSimpleAnyVal[A : HasId, K <: Symbol, T <: HList](
    implicit
    witness: Witness.Aux[K],
    P: Primitive[A],
    D: Lazy[FindDeltaMeta[T]],
  ): FindDeltaMeta[FieldType[K, A] :: T] =
    (a, b) => (
      if (P.eqv.eqv(a.head: A, b.head: A)) {
        Meta.empty
      } else {
        Meta(
          witness.value.name,
          Ior.Both(
            HasId[A].id(a.head: A),
            HasId[A].id(b.head: A)
          ),
          Nil
        )
      }) ++ D.value.apply(a.tail, b.tail)
}

trait LowPriorityInstances2 extends LowPriorityInstances3 {
  implicit def hListWithSimpleAnyEnum[A, K <: Symbol, T <: HList](
    implicit
    witness: Witness.Aux[K],
    IsAnyVal: A As EnumEntry,
    D: Lazy[FindDeltaMeta[T]],
  ): FindDeltaMeta[FieldType[K, A] :: T] =
    (a, b) => {
      val aEnum: EnumEntry = a.head.asInstanceOf[EnumEntry]
      val bEnum: EnumEntry = b.head.asInstanceOf[EnumEntry]

      if (aEnum.entryName === bEnum.entryName) {
        Meta.empty
      } else {
        Meta(
          witness.value.name,
          Ior.Both(
            aEnum.entryName,
            bEnum.entryName
          ),
          Nil
        )
      }
    } ++ D.value.apply(a.tail, b.tail)
}

trait LowPriorityInstances3 extends LowPriorityInstances4 {
  implicit def hLIstWithFofHListInsideItOption[A, K <: Symbol, H, InnerT <: HList, T <: HList](
    implicit
    witness: Witness.Aux[K],
    IsList: H As Option[A],
    eachH: LabelledGeneric.Aux[A, InnerT],
    HN: HasId[A],
    D: Lazy[FindDeltaMeta[T]],
    E: Lazy[FindDeltaMeta[InnerT]]
  ): FindDeltaMeta[FieldType[K, H] :: T] =
    (a, b) => {
      val leftOption = a.head.asInstanceOf[Option[A]]
      val rightOption = b.head.asInstanceOf[Option[A]]

      val r =
        (leftOption, rightOption) match {
          case (Some(la), Some(ra)) => E.value.apply(eachH.to(la), eachH.to(ra)).prependToKey(HasId[A].id(la))
          case (Some(la), None) => Meta(HasId[A].id(la), Ior.Left(la.toString), Nil)
          case (None, Some(ra)) => Meta(HasId[A].id(ra), Ior.Right(ra.toString), Nil)
          case _ => Meta.empty
        }

      r.prependToKey(witness.value.name) ++ D.value.apply(a.tail, b.tail)
    }
}

trait LowPriorityInstances4 extends LowPriorityInstances5 {
  implicit def hLIstWithFofHListInsideIt[A, K <: Symbol, H, InnerT <: HList, T <: HList](
    implicit
    witness: Witness.Aux[K],
    IsList: H As List[A],
    eachH: LabelledGeneric.Aux[A, InnerT],
    HN: HasId[A],
    D: Lazy[FindDeltaMeta[T]],
    E: Lazy[FindDeltaMeta[InnerT]]
  ): FindDeltaMeta[FieldType[K, H] :: T] =
    (a, b) => {
      val leftList = a.head.asInstanceOf[List[A]]
      val rightList = b.head.asInstanceOf[List[A]]
      val namesOnLeft = leftList.map(t => HasId[A].id(t))

      val toBeComparedOnRight =
        rightList.filter {
          bb => namesOnLeft.containsSlice(List(HasId[A].id(bb)))
        }

      // convert toString to shows and print it nicely
      val newData =
        rightList.filterNot(
          t => toBeComparedOnRight.map(t => HasId[A].id(t)).containsSlice(List(HasId[A].id(t)))
        ).map(t => Meta(HasId[A].id(t), Ior.Right(t.toString), Nil).prependToKey(witness.value.name))

      // convert toString to shows and print it nicely
      val deletedData =
        leftList.filterNot(t => rightList.map(t => HasId[A].id(t)).containsSlice(List(HasId[A].id(t))))
          .map(t => Meta(HasId[A].id(t), Ior.Left(t.toString), Nil).prependToKey(witness.value.name))

      val compareResourcesWithSameName =
        leftList.zip(toBeComparedOnRight).map {
          case (aa, bb) =>
            E.value.apply(eachH.to(aa), eachH.to(bb)).prependToKey(HasId[A].id(aa))
        }

      deletedData.fold(Nil)(_ ++ _) ++
        newData.fold(Nil)(_ ++ _) ++
        compareResourcesWithSameName.fold(Nil)(_ ++ _).prependToKey(witness.value.name) ++
        D.value.apply(a.tail, b.tail)
    }
}

trait LowPriorityInstances5 extends LowPriorityInstance6 {
  implicit def hListNamerWithHListInsideOfInsideOf[K <: Symbol, H, InnerT <: HList, T <: HList](
    implicit
    witness: Witness.Aux[K],
    eachH: LabelledGeneric.Aux[H, InnerT],
    H: HasId[H],
    D: Lazy[FindDeltaMeta[T]],
    E: Lazy[FindDeltaMeta[InnerT]]
  ): FindDeltaMeta[FieldType[K, H] :: T] =
    (a, b) => {

      val diff =
        if (H.id(a.head) === H.id(b.head)) {
          E.value.apply(eachH.to(a.head.asInstanceOf[H]), eachH.to(b.head.asInstanceOf[H]))
            .prependToKey(HasId[H].id(b.head)).prependToKey(witness.value.name)
        } else {
          // convert toString to shows and print it nicely
          Meta(HasId[H].id(b.head), Ior.Right(b.head.toString), Nil).prependToKey(witness.value.name) ++
            Meta(HasId[H].id(a.head), Ior.Left(a.head.toString), Nil).prependToKey(witness.value.name)
        }

      diff ++ D.value.apply(a.tail, b.tail)
    }
}

trait LowPriorityInstance6 {
  implicit def simpleHList[A, K <: Symbol, H: Eq, R <: HList, T <: HList](
    implicit
    witness: Witness.Aux[K],
    D: Lazy[FindDeltaMeta[T]],
  ): FindDeltaMeta[FieldType[K, H] :: T] =
    (a, b) => {
      (if ((a.head: H) === (b.head: H)) {
        Meta.empty
      } else {
        Meta(
          witness.value.name,
          Ior.Both((a.head: H).toString, (b.head: H).toString),
          Nil
        )
      }
        ) ++ D.value.apply(a.tail, b.tail)
    }
}
