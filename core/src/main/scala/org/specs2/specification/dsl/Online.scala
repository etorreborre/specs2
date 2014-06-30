package org.specs2
package specification
package dsl

import execute.AsResult
import specification.core.{FragmentsContinuation, Fragments}

trait Online {
  implicit class resultToFragmentsContinuation[R : AsResult](r: =>R) {
    def continueWith(fs: =>Fragments) =
      FragmentsContinuation.continueWith(r, fs)
  }
}

object Online extends Online
