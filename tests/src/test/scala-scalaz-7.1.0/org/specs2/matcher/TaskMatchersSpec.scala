package org.specs2
package matcher

import scalaz.concurrent.Task
import scala.concurrent.duration._

class TaskMatchersSpec extends Spec with TaskMatchers with ResultMatchers with Retries { def is = s2"""

 It is possible to check the execution of tasks
   check the return value
   ${ Task(1) must returnValue(1) }
   ${ (Task(1) must returnValue(2)) returns "'1' is not equal to '2'" }

   check that the task finishes before a given time
   ${ Task(1) must returnBefore(10.millis) }
   ${ (Task { Thread.sleep(50); 1 } must returnBefore(10.millis)) returns "Timeout after 10 milliseconds" }

   check the return value with a timeout
   ${ Task { Thread.sleep(5); 1 } must returnBefore(10.millis).withValue(1) }
   ${ Task { Thread.sleep(5); 1 } must returnValue(1).before(10.millis) }
"""
}
