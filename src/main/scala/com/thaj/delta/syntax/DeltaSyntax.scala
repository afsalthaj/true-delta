package com.thaj.delta
package syntax
import cats.data.Ior
import com.thaj.delta.Delta._
import com.thaj.delta.Delta.DeltaOp._
import com.thaj.delta.DeltaMeta.Meta

trait DeltaSyntax {
  implicit class DeltaOps[A](a: List[A]) {
    def diffWith(b: List[A])(implicit M: Delta[A], N: HasId[A]): List[DeltaOp[A]] =
      Delta.asIoR[A](a, b).map(M.delta)
  }

  implicit class DeltaOpListSyntax[A : HasId](a: List[DeltaOp[A]]) {
    // Avoid using shapeless for productName because we know the name of the case-class/product at this point.
    // Use shapeless/compile-time-reflections only if `A` is unknown at compile time.
    def asMeta(implicit P: ParentResourceName[A]): Meta  = a.flatMap {
      case Create(a1) => Meta(ParentResourceName[A].name(a1), Ior.Right(a1.toString), Nil)
      case Delete(a1) => Meta(ParentResourceName[A].name(a1), Ior.Left(a1.toString), Nil)
      case Update(a1, a2, meta) => Meta(ParentResourceName[A].name(a2), Ior.Both(a1.toString, a2.toString), meta)
      case NoChange(_) => Meta.empty
    }
  }
}

trait AllDeltaSyntax
  extends DeltaSyntax

object AllDeltaSyntax
  extends AllDeltaSyntax
