package org.specs2
package time

import specification.Grouped

class TimerSpec extends Specification with Grouped {  def is =

  "A timer can be created and not started"                                   ^
    "neverStarted returns true"                                              ! g1.e1^
    "isStarted returns false"                                                ! g1.e2^
    "it displays no elapsed time"                                            ^
      "in hms"                                                               ! g1.e3^
	  "in hms and millis"                                                      ! g1.e4^
	                                                                           endbr^
  "When started"                                                             ^
    "neverStarted returns false"                                             ! g2.e1^
    "isStarted returns true"                                                 ! g2.e2^
    "it displays no elapsed time"                                            ! g2.e3^
                                                                             p^
  "When stopped"                                                             ^
    "neverStarted returns false"                                             ! g3.e1^
    "isStarted returns false"                                                ! g3.e2^
	"it can"                                                                   ^
      "display the elapsed time in hour-minute-second"                       ! g3.e3^
      "display the elapsed time in hms and millis"                           ! g3.e4^
      "display the correct time when it is more than 1 hour"                 ! g3.e5^
                                                                             endbr^
  "A Timer can also have nested starts and stops"                            ^
    "it will then return cumulated times"                                    ! g4.e1^
                                                                             p^
  "2 Timers can be added together"                                           ^
    "if they both have elapsed times we take the maximum"                    ! g5.e1^
    "if they both have started timestamps we take the greatest difference"   ! g5.e2^
                                                                             end
    
  "timer is not started" - new g1 {
    val timer = TestTimer()

    e1 := timer.neverStarted must beTrue
  	e2 := timer.isStarted must beFalse
  	e3 := timer.hms must_== "0 second"
  	e4 := timer.time must_== "0 ms"
  }	
  
  "timer is started" - new g2 {
    val timer = TestTimer().start

    e1 := timer.neverStarted must beFalse
	  e2 := timer.isStarted must beTrue
	  e3 := timer.time must_== "0 ms"
  }	

  "timer is stopped" - new g3  {
    val timer = TestTimer().set(currentTime = 0L).start.
                            set(currentTime = 500L).stop
							
    e1 := timer.neverStarted must beFalse
	  e2 := timer.isStarted must beFalse
	
    e3 := TestTimer().set(currentTime = 1000L).start.
                      set(currentTime = 2000L).stop.hms must_== "1 second"
  
    e4 := TestTimer().set(currentTime = 1000L).start.
                      set(currentTime = 2500L).stop.time must beMatching("1 second, 500 ms")
                                                                                         
    e5 := TestTimer().set(currentTime = 0L).start.
                      set(currentTime = 3800010L).stop.time must beMatching("1 hour 3 minutes 20 seconds, 10 ms")
  }	

  "nested starts" - new g4 {
    e1 := TestTimer().set(currentTime = 1000L).start.
                      set(currentTime = 2000L).start.
                      set(currentTime = 3000L).stop.
                      set(currentTime = 4000L).stop.time must beMatching("3 seconds, 0 ms")
  }
    
  "times addition" - new g5 {
    e1 := (TestTimer().runFor(100L) add TestTimer().runFor(200L)).time === "200 ms"
    e2 := (TestTimer().set(currentTime = 300L).start add TestTimer().set(currentTime = 100L).start).time === "200 ms"
  }

	
	
  case class TestTimer(currentTime: Long = 0L) extends HmsTimer[TestTimer] { outer =>
    override def getTime = currentTime

    def runFor(t: Long) = copy(List(t), Nil)

    def set(currentTime: Long = 0L) = newTestTimer(currentTime, elapsedTimes, startedTimestamps)
    def copy(e: List[Long], m: List[Long]) = newTestTimer(currentTime, e, m)
    
    def newTestTimer(currentTime: Long, elapsed: List[Long], timestamps: List[Long]) =
      new TestTimer(currentTime) {
        override protected val elapsedTimes = elapsed
        override val startedTimestamps = timestamps
      }
  }
}
