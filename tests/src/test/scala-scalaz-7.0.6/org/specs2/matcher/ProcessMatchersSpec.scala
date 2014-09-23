package org.specs2
package matcher

import java.util.concurrent._
import scala.concurrent.duration._
import scalaz.concurrent._
import scalaz.stream._

class ProcessMatchersSpec extends Specification with ProcessMatchers with ResultMatchers with Retries { def is = s2"""

 It is possible to check the execution of processes
   check the produced values
   ${ oneElement(1) must returnValues(Seq(1)) }
   ${ (oneElement(1) must returnValues(IndexedSeq(2))) returns "Vector(1) is not equal to Vector(2)" }

   check the last value
   ${ oneElement(1) must returnLast(1) }
   ${ (oneElement(1) must returnLast(2)) returns "'1' is not equal to '2'" }
   ${ (oneElement(1).drop(1) must returnLast(1)) returns "Expected a value, got None" }

   check the last value as an option
   ${ oneElement(1).drop(1) must returnLastOption(None) }
   ${ oneElement(1) must returnLastOption(Some(1)) }
   ${ (oneElement(1) must returnLastOption(Some(2))) returns "'Some(1)' is not equal to 'Some(2)'" }

   ${step(scheduledExecutorService.shutdown)}
"""

  def oneElement[T](t: =>T) =
    Process.eval(Task.delay(t))

  def oneElementAfter[T](t: =>T, duration: FiniteDuration) =
    Process.sleep(duration) fby oneElement(t)

  implicit val scheduledExecutorService: ScheduledExecutorService =
    Executors.newScheduledThreadPool(1)
}