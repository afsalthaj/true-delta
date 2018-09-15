package com.thaj.delta.syntax

import com.thaj.delta.DeltaMeta.Meta

trait MetaSyntax {

  implicit class MetaOps(self: Meta) {
    def appendToKey(s: String): Meta =
      Meta.appendToKey(s, self)

    def prependToKey(s: String): Meta =
      Meta.prependToKey(s, self)
  }
}
