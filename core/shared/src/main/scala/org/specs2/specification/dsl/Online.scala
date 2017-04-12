package org.specs2
package specification
package dsl

import execute.AsResult
import org.specs2.specification.core.{Execution, FragmentsContinuation, Fragments}

/**
 * Syntax for continuing an example with a sub-specification depending on the example's success
 */
trait Online {
  implicit class resultToFragmentsContinuation[R : AsResult](r: =>R) {
    def continueWith(fs: =>Fragments): Execution =
      FragmentsContinuation.continueWith(r, fs)
  }
}

object Online extends Online
