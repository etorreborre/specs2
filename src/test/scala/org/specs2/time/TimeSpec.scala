package org.specs2
package time
import TimeConversions._
import Time._

class TimeSpec extends SpecificationWithJUnit { def is =
  
  "A duration can"                                                                        ^
    " be created for 1 minute"                                                            ^
    { 1.minute.inSeconds must_== 60 }                                                     ^
                                                                                          p^
    " be added to another duration"                                                       ^
    { (1.minute + 2.seconds).inSeconds must_== 62 }                                       ^
                                                                                          endp^
  "Time can"                                                                              ^
    " be frozen so different durations can safely refer to 'now'"                         !
    { Time.freeze; (1.minute.fromNow - Time.now).inSeconds must_== 60 }                   ^
                                                                                          end
}
