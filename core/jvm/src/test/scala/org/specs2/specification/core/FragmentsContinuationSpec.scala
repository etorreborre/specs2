package org.specs2
package specification
package core

import process.DefaultExecutor
import execute._
import org.specs2.matcher.ActionMatchers

class FragmentsContinuationSpec(val env: Env) extends Specification with ActionMatchers with OwnEnv { def is = s2"""

 A fragment continuation must
   return other fragments if the previous result is a success                $continuationAfterSuccess
   not return other fragments if the previous result is not a success        $continuationAfterFailure
   return an error fragment if the continuation fragments fail to be created $continuationError
"""

  def continuationAfterSuccess =
    runContinuation(ok, continuation = "continuation" ! ok) must haveSize(2)

  def continuationAfterFailure =
    runContinuation(ko, continuation = "continuation" ! ok) must haveSize(1)

  def continuationError = {
    val fragments = runContinuation(ok, continuation = {sys.error("boom"); "continuation" ! ok})
    (fragments must haveSize(2)) and
    (fragments(1).description.show must beMatching("Could not create fragments after the previous successful result"))
  }

  /** HELPERS */
  def runContinuation[R : AsResult](r1: =>R, continuation: =>Fragments): List[Fragment] = {
    Fragments(DefaultExecutor(ownEnv).execute(ownEnv.arguments)(
      Fragments("test" ! FragmentsContinuation.continueWith(r1, continuation)).contents)).
      fragmentsList(ownEnv.executionEnv)
  }
}
