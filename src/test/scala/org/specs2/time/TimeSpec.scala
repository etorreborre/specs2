package org.specs2
package time

class TimeSpec extends Specification { def is = s2"""
  
  A duration can
    be created for 1 minute
    ${ 1.minute.inSeconds must_== 60 }

    be added to another duration
    ${ (1.minute + 2.seconds).inSeconds must_== 62 }

  Time can
    be frozen so different durations can safely refer to 'now'
    ${ Time.freeze; (1.minute.fromNow - Time.now).inSeconds must_== 60 }

  Duration implicits can be deactivated with the NoTimeConversion trait $e1
                                                                                 """

  def e1 = {
    val spec = new Specification with NoTimeConversions {
      implicit def intToMyMillis(i: Int) = new {
        def millis = i * 2
      }
      val result = 1.millis === 2
      def is = result
    }
    spec.result
  }
}
