package com.thaj.delta

trait ParentName[A] {
  def parentName: String

  def name(a: A)(implicit H: HasId[A]): String =
    parentName + "." + HasId[A].id(a)
}

object ParentName {
  def apply[A](implicit ev: ParentName[A]): ParentName[A] = ev

  def createInstanceWithHasName[A : HasId](parentFieldName: String): ParentName[A] =
    new ParentName[A] {
      override def parentName: String = parentFieldName
    }

  def fromResourceName[A : ParentName](resourceName: String): String =
    ParentName[A].parentName + "." + resourceName
}
