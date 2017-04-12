package org.specs2
package specification
package core

import org.specs2.execute.{Error, AsResult, Result}
import control.Exceptions._

/**
 * Function creating more fragments (to be added to the specification)
 * based on the current result
 */
case class FragmentsContinuation(continue: Result => Option[Fragments]) {
  def apply(result: Result) = continue(result)
}

object FragmentsContinuation {
  /** create a continuation */
  def continueWith[R : AsResult](result: =>R, fs: =>Fragments): Execution = {
    def fragmentsCreationError(e: Throwable): Fragments =
      Fragments(
        Fragment(Text("Could not create fragments after the previous successful result"),
                 Execution.result(Error(e))))

    Execution(result, FragmentsContinuation { r: Result =>
      if (r.isSuccess) Some(tryOr(fs)(fragmentsCreationError))
      else None
    })
  }
}


