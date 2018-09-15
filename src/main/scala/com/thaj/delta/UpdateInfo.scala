package com.thaj.delta

import cats.data.Ior

/**
  * Recursive UpdateInfo (`reason`) conceptually says,
  * we can build an UpdateInfo that tracks the previousValue and newValue with a reason
  * for what resulted in previousValue and newValue, and that is a List[UpdateInfo].
  * The smart constructor allows you to create the reason only if there is a previousValue and newValue and
  * not when something is newly created or deleted.
  *
  * The stringified action should exist in only 1 place in the entire app. This can be ugly but we need only
  * string as output and nothing else.
  */
final case class UpdateInfo private (key: String, action: String, previousValue: String, newValue: String, reason: List[UpdateInfo])

object UpdateInfo {
  def mk(key: String, previousNewValue: String Ior String, reason: List[UpdateInfo]): UpdateInfo =
    previousNewValue match {
      case Ior.Both(a, b) => UpdateInfo(key, "update", a, b, reason)
      case Ior.Left(a) => UpdateInfo(key, "delete", a, "", Nil)
      case Ior.Right(b) => UpdateInfo(key, "create", "", b, Nil)
    }
}
