package org.specs2
package time
import specification._

class TimerSpec extends Specification { 
var timer = new SimpleTimer
def is =
  Step(timer = timer.start)^
  "A timer can be created and not started"                           ^
    "neverStarted returns true"                                      ! neverStarted.e1^
    "isStarted returns false"                                        ! neverStarted.e2^
    "it displays no elapsed time"                                    ^
      "in hms"                                                       ! neverStarted.e3^
	  "in hms and millis"                                            ! neverStarted.e4^
	                                                                 endp^
  "When started"                                                     ^
    "neverStarted returns false"                                     ! started.e1^
    "isStarted returns true"                                         ! started.e2^
    "it displays no elapsed time"                                    ! started.e3^
                                                                     p^
  "When stopped"                                                     ^
    "neverStarted returns false"                                     ! stopped.e1^
    "isStarted returns false"                                        ! stopped.e2^
	"it can"                                                          ^
      "display the elapsed time in hour-minute-second"               ! stopped.e3^
      "display the elapsed time in hms and millis"                   ! stopped.e4^
      "display the correct time when it is more than 1 hour"         ! stopped.e5^
                                                                     p^
  "A Timer can also have nested starts and stops"                    ^
    "it will then return cumulated times"                            ! nested.e1^
                                                                     p^
  "2 Timers can be added together"                                           ^
    "if they both have elapsed times we add those times"                     ! addition.e1^
    "if they both have nested starts we add those start times"               ! addition.e2^
    "if one is started and the other one neverStarted we stop the first one" ! addition.e3^
	Step(println(timer.stop.time))^
                                                                             end
    
  object neverStarted { val timer = TestTimer()
    def e1 = timer.neverStarted must beTrue
	def e2 = timer.isStarted must beFalse
	def e3 = timer.hms must_== "0 second"
	def e4 = timer.time must_== "0 ms"
  }	
  
  object started { val timer = TestTimer().start
    def e1 = timer.neverStarted must beFalse
	def e2 = timer.isStarted must beTrue
	def e3 = timer.time must_== "0 ms"
  }	

  object stopped  { 
    val timer = TestTimer().set(currentTime = 0L).start.
                            set(currentTime = 500L).stop
							
    def e1 = timer.neverStarted must beFalse
	def e2 = timer.isStarted must beFalse
	
    def e3 = TestTimer().set(currentTime = 1000L).start.
                         set(currentTime = 2000L).stop.
             hms must_== "1 second"
  
    def e4 = TestTimer().set(currentTime = 1000L).start.
                        set(currentTime = 2500L).stop.
             time must beMatching("1 second, 500 ms")
                                                                                         
    def e5 = TestTimer().set(currentTime = 0L).start.
                         set(currentTime = 3800010L).stop.
             time must beMatching("1 hour 3 minutes 20 seconds, 10 ms")
  }	

  object nested {

    def e1 = TestTimer().set(currentTime = 1000L).start.
                         set(currentTime = 2000L).start.
                         set(currentTime = 3000L).stop.
                         set(currentTime = 4000L).stop.
             time must beMatching("3 seconds, 0 ms")

  }
    
  object addition {

    def e1 = {
	  val timer1 = TestTimer().start.set(currentTime = 200L).stop
	  val timer2 = TestTimer().start.set(currentTime = 100L).stop
	  (timer1 add timer2).time === "300 ms"
	}
	def e2 = {
	  val timer1 = TestTimer().start.set(currentTime = 200L).start
	  val timer2 = TestTimer().start.set(currentTime = 100L).start
	  (timer1 add timer2).millis.size === 4
	}
	def e3 = {
	  val timer1 = TestTimer().start.set(currentTime = 200L)
	  val timer2 = TestTimer()
	  (timer1 add timer2).time === "200 ms"
	}

  }

	
	
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
