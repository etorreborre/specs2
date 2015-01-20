package org.specs2
package control

import Actions._
import scalaz.\&/.This
import scalaz._, Scalaz._
import matcher._
import MatchersImplicits._
import ExpectationsDescription._
import StatusT._

class ActionTSpec extends Specification { def is = s2"""

 Warnings can be accumulated with actions                $warning
 A warning can be emitted then fail the computation      $warningAndFail
 An action can be executed only when a condition is true $when

"""

  def warning = {
    val actions =
      Actions.ok(1) >>
      warn("not sure") >>
      Actions.ok(2)

    val (warnings, result) = actions.run(noLogging).unsafePerformIO

    { warnings must_== Vector("not sure") } and
    { result must_== Ok(2) }
  }

  def warningAndFail = {
    val actions =
      Actions.ok(1) >>
      warnAndFail("not sure", "Run aborted!") >>
      Actions.ok(2)

    val (warnings, result) = actions.run(noLogging).unsafePerformIO

    { warnings must_== Vector("not sure") } and
    { result must_== Ko(This("Run aborted!")) }
  }

  def when = {
    var i = 0
    Actions.safe { i = 1 }.when(false).run(noLogging).unsafePerformIO
    "the action is not executed" ==> { i must_== 0 }
  }

}
