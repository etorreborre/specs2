package org.specs2
package matcher

import io.StringOutput
import java.util.concurrent._
import org.specs2.concurrent.ExecutionEnv
import specification._
import scala.concurrent.duration._

class TerminationMatchersSpec extends script.Specification with TerminationMatchers with Grouped { def is = section("travis") ^ sequential ^ s2"""
                                                                                                   
 It is possible to check if a block of code terminates
   with a default number of retries and default sleep time
     + if it succeeds
     + if it fails

   with a specified number of retries and sleep time
     + if it succeeds after 50 ms
     + if it fails

   + if the termination fails, the computation is stopped
   + We can write 'action must not terminate'

 We should be able to observe that an action unblocks another
   + with a when clause
   with an onlyWhen clause
     + if action1 terminates after action 2 -> success
     + if action1 terminates before action 2 -> failure
     + if action1 terminates before action 2 -> failure with a specific message
     + if action1 doesn't terminate after action 2 -> failure

  We should not overflow the stack
    + when a very large number of retries is provided
                                                                                                """

  "termination" - new group {
    eg := { implicit ee: EE => sleepFor(50) must terminate(sleep = 200.millis) }
    eg := { implicit ee: EE => (sleepFor(300) must terminate(retries=1, sleep=100.millis)) returns "the action is blocking with retries=1 and sleep=100" }

    eg := { implicit ee: EE => sleepFor(50) must terminate(retries=3, sleep=20.millis) }
    eg := { implicit ee: EE => (sleepFor(1000) must terminate(retries=3, sleep=20.millis)) returns "the action is blocking with retries=3 and sleep=20" }

    eg := { implicit ee: EE =>
      val out = new StringOutput { }
      val terminated = (1 to 5).foreach (i => {sleepFor(80 * i.toLong); out.println(i) }) must not terminate(retries=5, sleep=20.millis)
      sleepFor(300) // wait until all the messages are possibly written to out if the action was not terminated
      terminated and (out.messages must not contain("3"))
    }
    eg := { implicit ee: EE => sleepFor(150) must not terminate }

    eg := { implicit ee: EE =>
      val queue = new ArrayBlockingQueue[Int](1)
      queue.take() must terminate.when("adding an element", queue.add(1))
    }

    eg := { implicit ee: EE =>

      val queue1 = new ArrayBlockingQueue[Int](1)
      var stop = true
      def action1() = scalaz.concurrent.Future({ while (stop) { sleepFor(10)}; queue1.add(1) }).unsafePerformSync
      def action2() = scalaz.concurrent.Future({ stop = false }).unsafePerformSync

      action1() must terminate.onlyWhen(action2())
    }

    eg := { implicit ee: EE =>
      val queue1 = new ArrayBlockingQueue[Int](1)

      (queue1.add(1) must terminate.onlyWhen(queue1.size)) returns "the action terminated before the second action"
    }

    eg := { implicit ee: EE =>
      val queue1 = new ArrayBlockingQueue[Int](1)

      (queue1.add(1) must terminate.onlyWhen("taking the size", queue1.size)) returns "the action terminated before taking the size"
    }

    eg := { implicit ee: EE =>
      val queue1 = new ArrayBlockingQueue[Int](1)
      // we sleep first for 100, then trigger the action and wait again for 100. In that case, it's not enough waiting
      // even after the action has been triggered
      ({sleepFor(300); queue1.add(1)} must terminate.onlyWhen(queue1.add(1))) returns "the action is blocking with retries=1 and sleep=100"
    }

    eg := { implicit ee: EE =>
      import scala.concurrent.duration._ //need to convert a Double to millis so that the test runs quickly
      // we sleep for 10 seconds
      // we retry 100,000 times with a sleep of 0.01 millis
      // thus we must terminate within (100,000 * 0.01) ms = 1 second
      // which will not happen since we sleep for 10 seconds
      sleepFor(10 * 1000) must terminate(retries=100000, sleep=0.01.millis) returns "the action is blocking with retries=100000 and sleep=0"
    }

  }

  type EE = ExecutionEnv

  def sleepFor(duration: Long) =
    try Thread.sleep(duration) catch { case t: Throwable => () }

}
