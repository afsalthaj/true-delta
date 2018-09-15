package com.thaj.delta

trait ParentResourceName[A] {
  def parentName: String

  def name(a: A)(implicit H: HasId[A]): String =
    parentName + "." + HasId[A].id(a)
}

object ParentResourceName {
  def apply[A](implicit ev: ParentResourceName[A]): ParentResourceName[A] = ev

  def createInstanceWithHasName[A : HasId](parentFieldName: String): ParentResourceName[A] =
    new ParentResourceName[A] {
      override def parentName: String = parentFieldName
    }

  def fromResourceName[A : ParentResourceName](resourceName: String): String =
    ParentResourceName[A].parentName + "." + resourceName
}
