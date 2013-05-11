package org.specs2
package matcher

import io.StringOutput
import java.util.concurrent._
import scalaz.concurrent.Promise
import specification._

class TerminationMatchersSpec extends script.Specification with TerminationMatchers with Grouped { def is = section("unstable")^ s2"""
                                                                                                   
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
     + if action1 doesn't terminate after action 2 -> failure
                                                                                                """

  "termination" - new group {
    eg := { Thread.sleep(50) must terminate(sleep = 100.millis) }
    eg := { (Thread.sleep(150) must terminate) returns "the action is blocking with retries=1 and sleep=100" }

    eg := { Thread.sleep(50) must terminate(retries=3, sleep=20.millis) }
    eg := { (Thread.sleep(1000) must terminate(retries=3, sleep=20.millis)) returns "the action is blocking with retries=3 and sleep=20" }

    eg := {
      val out = new StringOutput { }
      val terminated = (1 to 5).foreach (i => {Thread.sleep(80 * i); out.println(i) }) must not terminate(retries=5, sleep=20.millis)
      Thread.sleep(300) // wait until all the messages are possibly written to out if the action was not terminated
      terminated and (out.messages must not contain("3"))
    }
    eg := { Thread.sleep(150) must not terminate }

    eg := {
      val queue = new ArrayBlockingQueue[Int](1)
      queue.take() must terminate.when("adding an element", queue.add(1))
    }

    eg := {
      val queue1 = new ArrayBlockingQueue[Int](1)
      var stop = true
      def action1 = Promise { while (stop) { Thread.sleep(10)}; queue1.add(1) }.get
      def action2 = Promise { stop = false }.get
      action1 must terminate.onlyWhen(action2)
    }

    eg := {
      val queue1 = new ArrayBlockingQueue[Int](1)

      ((queue1.add(1) must terminate.onlyWhen(queue1.size)) returns "the action terminated before the second action") and
      ((queue1.add(1) must terminate.onlyWhen("taking the size", queue1.size)) returns "the action terminated before taking the size")
    }

    eg := {
      val queue1 = new ArrayBlockingQueue[Int](1)
      // we sleep first for 100, then trigger the action and wait again for 100. In that case, it's not enough waiting
      // even after the action has been triggered
      ({Thread.sleep(300); queue1.add(1)} must terminate.onlyWhen(queue1.add(1))) returns "the action is blocking with retries=1 and sleep=100"
    }

  }
}