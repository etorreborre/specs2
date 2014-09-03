package org.specs2
package matcher

import scala.concurrent.duration._
import scalaz.concurrent._
import scalaz.stream._

class ProcessMatchersSpec extends Specification with ProcessMatchers with ResultMatchers { def is = s2"""

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

   check that the process finishes before a given time
   ${ oneElement(1) must terminateBefore(10.millis) }
   ${ (oneElementAfter(1, 5.millis) must terminateBefore(10.millis)) returns "Timeout after 10 milliseconds" } $xtag

   check the produced value with a timeout
   ${ oneElementAfter(1, 5.millis) must terminateBefore(10.millis).withValues(Seq(1)) }
   ${ oneElementAfter(1, 50.millis) must returnValues(Seq(1)).before(10.millis) }
"""
  
  def oneElement[T](t: =>T) =
    Process.eval(Task.delay(t))

  def oneElementAfter[T](t: =>T, duration: Duration) =
    Process.every(duration).map(_ => t).drop(1).take(1)
}
