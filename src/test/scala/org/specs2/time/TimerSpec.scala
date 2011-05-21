package org.specs2
package time

class TimerSpec extends Specification { def is =

  "A timer should"                                                                                                      ^
    "display 0 seconds if not stopped after being started"                                                              ! e1^
    "not display 0 seconds if there are millis"                                                                         ! e1_1^
                                                                                                                        p^
  "When started and stopped, it can"                                                                                    ^
    "display the elapsed time in hour-minute-second"                                                                    ! e2^
    "display the elapsed time in hms and millis"                                                                        ! e3^
                                                                                                                        p^
  "A Timer can also have nested starts and stops"                                                                       ^
    "it will then return cumulated times"                                                                               ! e4^
                                                                                                                        end
    
  def e1 = TestTimer().start.hms must_== "0 second"
  def e1_1 = TestTimer().set(currentTime = 0L).start.
                         set(currentTime = 500L).stop.
           time must_== "500 ms"

  def e2 = TestTimer().set(currentTime = 1000L).start.
                       set(currentTime = 2000L).stop.
           hms must_== "1 second"
  
  def e3 = TestTimer().set(currentTime = 1000L).start.
                       set(currentTime = 2500L).stop.
           time must beMatching("1 second, 500 ms")
                                                                                         
  def e4 = TestTimer().set(currentTime = 1000L).start.
                       set(currentTime = 2000L).start.
                       set(currentTime = 3000L).stop.
                       set(currentTime = 4000L).stop.
           time must beMatching("3 seconds, 0 ms")
    

  case class TestTimer(currentTime: Long = 0L) extends HmsTimer[TestTimer] { outer =>
    override def getTime = currentTime
    
    def set(currentTime: Long = 0L) = newTestTimer(currentTime, elapsed, millis) 
    def copy(e: Long, m: List[Long]) = newTestTimer(currentTime, e, m)
    
    def newTestTimer(currentTime: Long, e: Long, m: List[Long]) =
      new TestTimer(currentTime) {
        override val elapsed = e
        override val millis = m
      }
  }
}
