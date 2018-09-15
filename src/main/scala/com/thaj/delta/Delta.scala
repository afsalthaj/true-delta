package com.thaj.delta

import cats.data.Ior
import cats.implicits._
import com.thaj.delta.Delta.DeltaOp
import com.thaj.delta.Delta.DeltaOp._
import com.thaj.delta.DeltaMeta.Meta

import scala.annotation.implicitNotFound

@implicitNotFound(
  msg = "HINT: Ideally, if there is FindDeltaMeta[A], then we get Delta[A] in scope. " +
    "You can simply verify this by typing `FindDeltaMeta[A]` anywhere in the app which gives further information."
)
trait Delta[A] {
  def delta: A Ior A => DeltaOp[A]
}

object Delta {
  def apply[A](implicit ev: Delta[A]): Delta[A] = ev

  sealed trait DeltaOp[A]

  object DeltaOp {
    final case class Create[A](a: A) extends DeltaOp[A]
    final case class Update[A](a: A, b: A, meta: Meta) extends DeltaOp[A]
    final case class NoChange[A](a: A) extends DeltaOp[A]
    final case class Delete[A](a: A) extends DeltaOp[A]
  }

  def createInstance[A](f: (A, A) => Either[Update[A], NoChange[A]]): Delta[A] =
    new Delta[A] {
      override def delta: A Ior A => DeltaOp[A] = {
        case Ior.Both(a, b) => f(a, b) match {
          case Right(r) => NoChange(r.a)
          case Left(r) => Update(r.a, r.b, r.meta)
        }
        case Ior.Left(a) => Delete(a)
        case Ior.Right(b) => Create(b)
      }
    }

  implicit def `Delta from FindDeltaMeta`[A](implicit F: FindDeltaMeta[A]): Delta[A] =
    Delta.createInstance {
      (a1, a2) => {
        val meta = F(a1, a2)
        if (meta.nonEmpty) DeltaOp.Update[A](a1, a2, meta).asLeft else DeltaOp.NoChange[A](a1).asRight
      }
    }

  private[delta] def asIoR[A](a: List[A], b: List[A])(implicit N: HasId[A]): List[A Ior A] = {
    val left =
      a.map(aa => b.find(bb => N.id(bb) === N.id(aa))
        .fold[Ior[A, A]](Ior.Left(aa))(bb => Ior.Both(aa, bb)))

    left ++ b.filterNot(b => a.exists(aa => N.id(aa) === N.id(b))).map(Ior.Right(_))
  }
}
