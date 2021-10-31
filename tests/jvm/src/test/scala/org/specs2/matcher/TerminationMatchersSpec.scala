package org.specs2
package matcher

import java.util.concurrent.ArrayBlockingQueue
import scala.concurrent.*, duration.*

import io.StringOutput
import specification.core.{Env, OwnExecutionEnv}

class TerminationMatchersSpec(val env: Env) extends Specification with TerminationMatchers with OwnExecutionEnv {
  def is = section("ci") ^ sequential ^ s2"""

 It is possible to check if a block of code terminates
   with a default number of retries and default sleep time
     if it succeeds                                                              $termination1
     if it fails                                                                 $termination2

   with a specified number of retries and sleep time
     if it succeeds after 50 ms                                                  $termination3
     if it fails                                                                 $termination4

   if the termination fails, the computation is stopped                          $termination5
   We can write 'action must not(terminate)'                                     $termination6

 We should be able to observe that an action unblocks another
   with a when clause                                                            $termination7
   with an onlyWhen clause
     if Action terminates after action 2 -> success                              $termination8
     if Action terminates before action 2 -> failure                             $termination9
     if Action terminates before action 2 -> failure with a specific message     $termination10
     if Action doesn't terminate after action 2 -> failure                       $termination11

  We should not overflow the stack
    when a very large number of retries is provided                              $termination12
 """

  def termination1 = { sleepFor(50) must terminate(sleep = 200.millis) }
  def termination2 = {
    (sleepFor(300) must terminate(
      retries = 1,
      sleep = 100.millis
    )) returns "the action is blocking with retries=1 and sleep=100"
  }
  def termination3 = { sleepFor(50) must terminate(retries = 3, sleep = 20.millis) }
  def termination4 = {
    (sleepFor(1000) must terminate(
      retries = 3,
      sleep = 20.millis
    )) returns "the action is blocking with retries=3 and sleep=20"
  }

  def termination5 =
    val out = new StringOutput {}
    val terminated = (1 to 5).foreach(i => { sleepFor(80 * i.toLong); out.println(i) }) must not(
      terminate(retries = 5, sleep = 20.millis)
    )
    sleepFor(300) // wait until all the messages are possibly written to out if the action was not terminated
    terminated and (out.messages must not(contain("3")))

  def termination6 = { sleepFor(150) must not(terminate) }

  def termination7 =
    val queue = new ArrayBlockingQueue[Int](1)
    queue.take() must terminate.when("adding an element", queue.add(1))

  def termination8 =
    val queue1 = new ArrayBlockingQueue[Int](1)
    var stop = true
    def action1 = Await.result(Future({ while stop do { sleepFor(10) }; queue1.add(1) }), Duration.Inf)
    def action2 = Await.result(Future({ sleepFor(10); stop = false; 1 }), Duration.Inf)
    action1 must terminate.onlyWhen(action2)

  def termination9 =
    val queue1 = new ArrayBlockingQueue[Int](1)
    (queue1.add(1) must terminate.onlyWhen(queue1.size)) returns "the action terminated before the second action"

  def termination10 =
    val queue1 = new ArrayBlockingQueue[Int](1)
    (queue1.add(1) must terminate.onlyWhen(
      "taking the size",
      queue1.size
    )) returns "the action terminated before taking the size"

  def termination11 =
    val queue1 = new ArrayBlockingQueue[Int](1)
    // we sleep first for 100, then trigger the action and wait again for 100. In that case, it's not enough waiting
    // even after the action has been triggered
    ({ sleepFor(300); queue1.add(1) } must terminate.onlyWhen(
      queue1.add(1)
    )) returns "the action is blocking with retries=1 and sleep=100"

  def termination12 =
    import scala.concurrent.duration.* // need to convert a Double to millis so that the test runs quickly
    // we sleep for 10 seconds
    // we retry 100,000 times with a sleep of 0.01 millis
    // thus we must terminate within (100,000 * 0.01) ms = 1 second
    // which will not happen since we sleep for 10 seconds
    sleepFor(10 * 1000) must terminate(
      retries = 100000,
      sleep = 0.01.millis
    ) returns "the action is blocking with retries=100000 and sleep=0"

  // HELPERS
  def sleepFor(duration: Long) =
    try Thread.sleep(duration)
    catch { case t: Throwable => () }

}
