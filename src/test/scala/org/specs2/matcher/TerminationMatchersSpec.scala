package org.specs2
package matcher

import io.MockOutput
import java.util.concurrent._
import internal.scalaz.concurrent.Promise

class TerminationMatchersSpec extends Specification with TerminationMatchers { def is =

  "It is possible to check if a block of code terminates"                                          ^
    "with a default number of retries and default sleep time"                                      ^
      "if it succeeds"                                                                             ! e1^
      "if it fails"                                                                                ! e2^
                                                                                                   p^
    "with a specified number of retries and sleep time"                                            ^
      "if it succeeds"                                                                             ! e3^
      "if it fails"                                                                                ! e4^
                                                                                                   p^
    "if the termination fails, the computation is stopped"                                         ! e5^
                                                                                                   p^
    "We can write 'action must not terminate'"                                                     ! e6^
                                                                                                   p^
  "We should be able to observe that an action unblocks another"                                   ^
    "with a when clause"                                                                           ! e7^
    "with an onlyWhen clause"                                                                      ^
      "if action1 terminates after action 2 -> success"                                            ! e8^
      "if action1 terminates before action 2 -> failure"                                           ! e8_1^
      "if action1 doesn't terminate after action 2 -> failure"                                     ! e8_2^
    "with a list of actions"                                                                       ! e9^
                                                                                                   end

  def e1 = { Thread.sleep(50) must terminate }
  def e2 = { (Thread.sleep(150) must terminate) returns "the action is blocking with retries=1 and sleep=100" }
  def e3 = { Thread.sleep(50) must terminate(retries=3, sleep=20.millis) }
  def e4 = { (Thread.sleep(1000) must terminate(retries=3, sleep=20.millis)) returns "the action is blocking with retries=3 and sleep=20" }
  def e5 = {
    val out = new MockOutput { }
    val terminated = (1 to 5).foreach (i => {Thread.sleep(50); out.println(i) }) must not terminate(retries=5, sleep=20.millis)
    Thread.sleep(300) // wait until all the messages are possibly written to out if the action was not terminated
    terminated and (out.messages must not contain("3"))
  }
  def e6 = Thread.sleep(150) must not terminate

  def e7 = {
    val queue = new ArrayBlockingQueue[Int](1)
    queue.take() must terminate.when("adding an element", queue.add(1))
  }

  def e8 = {
    val queue1 = new ArrayBlockingQueue[Int](1)
    var stop = true
    def action1 = Promise { while (stop) { Thread.sleep(10)}; queue1.add(1) }.get
    def action2 = Promise { stop = false }.get
    action1 must terminate.onlyWhen(action2)
  }

  def e8_1 = {
    val queue1 = new ArrayBlockingQueue[Int](1)

    ((queue1.add(1) must terminate.onlyWhen(queue1.size)) returns "the action terminated before the second action") and
    ((queue1.add(1) must terminate.onlyWhen("taking the size", queue1.size)) returns "the action terminated before taking the size")
  }

  def e8_2 = {
    val queue1 = new ArrayBlockingQueue[Int](1)
    // we sleep first for 100, then trigger the action and wait again for 100. In that case, it's not enough waiting
    // even after the action has been triggered
    ({Thread.sleep(300); queue1.add(1)} must terminate.onlyWhen(queue1.add(1))) returns "the action is blocking with retries=1 and sleep=100"
  }

  def e9 = {
    val queue = new ArrayBlockingQueue[Int](1)
    val actions = Seq(() => { Thread.sleep(10); queue.take() }, () => { Thread.sleep(50); queue.add(1) }).par
    actions.map(_()).seq must terminate(sleep=200.millis)
  }

}