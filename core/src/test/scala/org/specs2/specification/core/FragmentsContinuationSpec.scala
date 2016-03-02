package org.specs2
package specification
package core

import process.DefaultExecutor
import execute._

class FragmentsContinuationSpec(env: Env) extends Specification { def is = s2"""

 A fragment continuation must
   return other fragments if the previous result is a success         $continuationSuccess
   not return other fragments if the previous result is not a success $continuationFailure

"""

  def continuationSuccess =
    runContinuation(ok, ko) must haveSize(2)

  def continuationFailure =
    runContinuation(ko, ko) must haveSize(1)

  def runContinuation[R : AsResult](r: =>R, fs: =>Fragments): IndexedSeq[Fragment] =
    Fragments(DefaultExecutor.execute1(env)(Fragments("test" ! FragmentsContinuation.continueWith(r, fs)).contents)).fragments
}
