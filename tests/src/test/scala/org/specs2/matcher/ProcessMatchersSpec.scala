package org.specs2
package matcher

import java.util.concurrent._

import execute.{AsResult, EventuallyResults}
import specification._
import time.NoTimeConversions

import scala.concurrent.duration._
import scalaz.concurrent._
import scalaz.stream._

class ProcessMatchersSpec extends Specification with ProcessMatchers with ResultMatchers with NoTimeConversions with Retries { def is = s2"""

 It is possible to check the execution of processes
   check the produced values
   ${ oneElement(1) must returnValues(Seq(1)) }
   ${ oneElement(1) must returnValues { vs: Seq[Int] => vs must_== Seq(1) } }
   ${ (oneElement(1) must returnValues(IndexedSeq(2))) returns "Vector(1) is not equal to Vector(2)" }

   check the last value
   ${ oneElement(1) must returnLast(1) }
   ${ oneElement(1) must returnLast((i: Int) => i must_== 1) }
   ${ (oneElement(1) must returnLast(2)) returns "'1' is not equal to '2'" }
   ${ (oneElement(1).drop(1) must returnLast(1)) returns "Expected a value, got None" }

   check the last value as an option
   ${ oneElement(1).drop(1) must returnLastOption(None) }
   ${ oneElement(1) must returnLastOption(Some(1)) }
   ${ oneElement(1) must returnLastOption((i: Option[Int]) => i must beSome(1)) }
   ${ (oneElement(1) must returnLastOption(Some(2))) returns "'Some(1)' is not equal to 'Some(2)'" }

   check that the process finishes before a given time
   ${ oneElement(1) must terminateBefore(10.millis) }
   ${ (oneElementAfter(1, 100.millis) must terminateBefore(50.millis)) returns "Timeout after 50 milliseconds" } $xtag

   ${Step(scheduledExecutorService.shutdown)}
"""

  def oneElement[T](t: =>T) =
    Process.eval(Task.delay(t))

  def oneElementAfter[T](t: =>T, duration: FiniteDuration) =
    Process(Thread.sleep(duration.toMillis)) fby oneElement(t)

  implicit val scheduledExecutorService: ScheduledExecutorService =
    Executors.newScheduledThreadPool(1)
}

trait Retries extends AroundExample with EventuallyResults {
  def retries: Int = 5
  def sleep: Duration = 100.millis

  // if the ci server is very loaded the tests might fail, so we retry 5 times
  def around[R : AsResult](r: =>R) =
    AsResult(eventually(retries = retries, sleep = org.specs2.time.Duration.fromScalaDuration(sleep))(r))

}
