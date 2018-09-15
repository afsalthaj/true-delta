package com.thaj.delta

import cats.data.Ior

object DeltaMeta {
  type Meta = List[UpdateInfo]

  object Meta {
    /**
      * To have the niceness of:
      * Meta("key", "old", "new") ++ Meta("anotherkey", "old", "new") ++  Meta("anotherHighLevelKey", "old", "new", meta)
      */
    def apply(key: String, previousAndNewValue: String Ior String, reason: List[UpdateInfo]): Meta =
      List(UpdateInfo.mk(key, previousAndNewValue, reason))

    def prependToKey(s: String, ma: Meta): Meta =
      ma.map { u => u.copy(key = s + "." + u.key) }

    def appendToKey(s: String, ma: Meta): Meta =
      ma.map { u => u.copy(key = u.key + "." + s) }

    def empty: Meta = Nil

  }
}
