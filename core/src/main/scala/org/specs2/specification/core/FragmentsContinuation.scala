package org.specs2
package specification
package core

import execute.{AsResult, Result}

case class FragmentsContinuation(continue: Result => Option[Fragments]) {
  def apply(result: Result) = continue(result)
}

object FragmentsContinuation {
  def continueWith[R : AsResult](result: =>R, fs: =>Fragments): Execution =
    Execution(result, FragmentsContinuation((r: Result) => Some(fs)))
}


