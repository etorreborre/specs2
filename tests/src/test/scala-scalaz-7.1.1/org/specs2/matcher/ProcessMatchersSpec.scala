package org.specs2
package matcher

import java.util.concurrent._
import scala.concurrent.duration._
import scalaz.concurrent._
import scalaz.stream._

class ProcessMatchersSpec extends mutable.Specification with ProcessMatchers with ResultMatchers with Retries {

  "It is possible to check the execution of processes" >> {
    "check the produced values" >> {
      { oneElement(1) must returnValues(Seq(1)) }
      { (oneElement(1) must returnValues(IndexedSeq(2))) returns "Vector(1) is not equal to Vector(2)" }
    }


    "check the last value" >> {
      { oneElement(1) must returnLast(1) }
      { (oneElement(1) must returnLast(2)) returns "'1' is not equal to '2'" }
      { (oneElement(1).drop(1) must returnLast(1)) returns "Expected a value, got None" }
    }

    "check the last value as an option" >> {
      { oneElement(1).drop(1) must returnLastOption(None) }
      { oneElement(1) must returnLastOption(Some(1)) }
      { (oneElement(1) must returnLastOption(Some(2))) returns "'Some(1)' is not equal to 'Some(2)'" }
    }

    "check that the process finishes before a given time" >> {
      { oneElement(1) must terminateBefore(10.millis) }
      { (oneElementAfter(1, 100.millis) must terminateBefore(50.millis)) returns "Timeout after 50 milliseconds" }
    }

    "check the produced value with a timeout" >> {
      { oneElementAfter(1, 10.millis) must terminateBefore(100.millis).withValues(Seq(1)) }
      { oneElementAfter(1, 10.millis) must returnValues(Seq(1)).before(100.millis) }
    }
  }

  step(scheduledExecutorService.shutdown)


  def oneElement[T](t: =>T) =
    Process.eval(Task.delay(t))

  def oneElementAfter[T](t: =>T, duration: FiniteDuration) =
    scalaz.stream.time.sleep(duration) fby oneElement(t)

  implicit val scheduledExecutorService: ScheduledExecutorService =
    Executors.newScheduledThreadPool(1)
}
