package org.specs2
package specification
package core

import execute.{AsResult, Result}

/**
 * Function creating more fragments (to be added to the specification)
 * based on the current result
 */
case class FragmentsContinuation(continue: Result => Option[Fragments]) {
  def apply(result: Result) = continue(result)
}

object FragmentsContinuation {
  /** create a continuation */
  def continueWith[R : AsResult](result: =>R, fs: =>Fragments): Execution =
    Execution(result, FragmentsContinuation((r: Result) => if (r.isSuccess) Some(fs) else None))
}


