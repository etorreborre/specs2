package org.specs2
package specification
package core

import execute.Result


case class FragmentsContinuation(continue: Result => Option[Fragments]) {
  def apply(result: Result) = continue(result)
}


