package org.specs2
package time

import matcher.TypedEqual

class TimerSpec extends Spec with TypedEqual {
  def is = sequential ^ s2"""

  A timer can be created and not started
    neverStarted returns true                                              $timer1
    isStarted returns false                                                $timer2
    it displays no elapsed time
      in hms                                                               $timer3
    in hms and millis                                                      $timer4

  When started
    neverStarted returns false                                             $started1
    isStarted returns true                                                 $started2
    it displays no elapsed time                                            $started3

  When stopped
    neverStarted returns false                                             $stopped1
    isStarted returns false                                                $stopped2
  it can
      display the elapsed time in hour-minute-second                       $stopped3
      display the elapsed time in hms and millis                           $stopped4
      display the correct time when it is more than 1 hour                 $stopped5

  A Timer can also have nested starts and stops
    it will then return cumulated times                                    $nested1

  2 Timers can be added together
    if they both have elapsed times we take the maximum                    $add1
    if they both have started timestamps we take the greatest difference   $add2
"""

  val timer = TestTimer()

  def timer1 = timer.neverStarted must beTrue
  def timer2 = timer.isStarted must beFalse
  def timer3 = timer.hms must ===("0 second")
  def timer4 = timer.time must ===("0 ms")

  val started = TestTimer().start

  def started1 = started.neverStarted must beFalse
  def started2 = started.isStarted must beTrue
  def started3 = started.time must ===("0 ms")

  val stopped = TestTimer().set(currentTime = 0L).start.set(currentTime = 500L).stop

  def stopped1 = stopped.neverStarted must beFalse
  def stopped2 = stopped.isStarted must beFalse
  def stopped3 = TestTimer().set(currentTime = 1000L).start.set(currentTime = 2000L).stop.hms must ===("1 second")

  def stopped4 =
    TestTimer().set(currentTime = 1000L).start.set(currentTime = 2500L).stop.time must beMatching("1 second, 500 ms")

  def stopped5 = TestTimer().set(currentTime = 0L).start.set(currentTime = 3800010L).stop.time must beMatching(
    "1 hour 3 minutes 20 seconds, 10 ms"
  )

  def nested1 = TestTimer()
    .set(currentTime = 1000L)
    .start
    .set(currentTime = 2000L)
    .start
    .set(currentTime = 3000L)
    .stop
    .set(currentTime = 4000L)
    .stop
    .time must beMatching("3 seconds, 0 ms")

  def add1 = (TestTimer().runFor(100L) `add` TestTimer().runFor(200L)).time === "200 ms"
  def add2 =
    (TestTimer().set(currentTime = 300L).start `add` TestTimer().set(currentTime = 100L).start).time === "200 ms"

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
