package org.specs2
package time
import TimeConversions._
import Time._

class TimeSpec extends SpecificationWithJUnit { def is =
  
  "A duration can"                                                                        ^
    " be created for 1 minute"                                                             ^
    { 1.minute.inSeconds must_== 60 }                                                     ^
    " be added to another duration"                                                        ^
    { (1.minute + 2.seconds).inSeconds must_== 62 }                                       ^
                                                                                          p^
  "Time can"                                                                              ^
    " be frozen so different durations can safely refer to 'now'"                          !
    { Time.freeze; (1.minute.fromNow - Time.now).inSeconds must_== 60 }                   ^
                                                                                          end
}
