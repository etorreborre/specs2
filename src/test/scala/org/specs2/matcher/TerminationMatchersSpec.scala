package org.specs2
package matcher

import io.MockOutput
import scala.actors.Futures._
import java.util.concurrent._

class   TerminationMatchersSpec extends Specification with TerminationMatchers { def is =

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
    "with an onlyWhen clause"                                                                      ! e8^
    "with a list of actions"                                                                       ! e9^
                                                                                                   end

  def e1 = { Thread.sleep(50) must terminate }
  def e2 = { (Thread.sleep(150) must terminate) returns "the action is blocking with retries=0 and sleep=100" }
  def e3 = { Thread.sleep(50) must terminate(retries=2, sleep=20.millis) }
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
    val queue = new ArrayBlockingQueue[Int](1)
    val noMessage = (queue.add(1) must terminate.onlyWhen(queue.add(1))) returns "the action terminates before the second action"
    val message = (queue.add(1) must terminate.onlyWhen("adding an element", queue.add(1))) returns "the action terminates before adding an element"
    noMessage and message
  }

  def e9 = {
    val queue = new ArrayBlockingQueue[Int](1)
    val actions = Seq(() => { Thread.sleep(10); queue.take() }, () => { Thread.sleep(50); queue.add(1) }).par
    actions.map(_()).seq must terminate(sleep=200.millis)
  }

}