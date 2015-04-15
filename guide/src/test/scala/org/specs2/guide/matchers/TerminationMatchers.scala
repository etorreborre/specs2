package org.specs2
package guide
package matchers

import org.specs2.concurrent.ExecutionEnv
import scala.concurrent.duration._

object TerminationMatchers extends UserGuideCard with matcher.TerminationMatchers {
  def title = "Termination"
  def text = s2"""

Sometimes you just want to specify that a block of code is going to terminate. The `${fullName[matcher.TerminationMatchers]}` trait is here to help. If you mix in that trait, you can write: ${snippet{
// 8<----
implicit val ee: ExecutionEnv = ??? // import your own
// 8<----
Thread.sleep(100) must terminate

// the default is retries = 0, sleep = 100.millis
Thread.sleep(100) must terminate(retries = 1, sleep = 60.millis)

}}

Note that the behaviour of this matcher is a bit different from the `eventually` operator. In this case, we let the current Thread sleep during the given `sleep` time and then we check if the computation is finished, then, we retry for the given number of `retries`.

In a further scenario, we might want to check that triggering another action is able to unblock the first one: ${snippet{
// 8<----
implicit val ee: ExecutionEnv = ??? // import your own
// 8<----
action1 must terminate.when(action2)
action1 must terminate.when("starting the second action", action2)
action1 must terminate(retries=3, sleep=100.millis).when(action2)
}}

When a second action is specified like that, `action1` will be started and `action2` will be started on the first retry. Otherwise, if you want to specify that `action1` can *only* terminate when `action2` is started, you write: ${snippet{
// 8<----
implicit val ee: ExecutionEnv = ??? // import your own
// 8<----
action1 must terminate.onlyWhen(action2)
}}

#### ExecutionEnv

The `terminate` matcher needs an implicit `ExecutionEnv` to be used. See the $ExecutionEnvironments page to learn how to get one.

"""
  lazy val (action1, action2) = ("", "")
}
