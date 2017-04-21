package org.specs2
package time

import java.util.Calendar

import text.Plural._
import control.Exceptions._
import org.specs2.control.origami.Fold
import org.specs2.fp._

/**
 * This trait provides Timer functionalities based on the Java Calendar milliseconds
 */
trait HmsTimer[T <: HmsTimer[T]] {
  /** elapsed times since for each stop */
  protected val elapsedTimes: List[Long] = Nil
  /** each time the timer is started we add the current time to this list of times number of millis when instantiating the object using this Trait */
  val startedTimestamps: List[Long] = Nil

  def copy(elapsed: List[Long], started: List[Long]): T
  /**
   * starts the with new elapsed time
   */
  def start = copy(Nil, getTime :: startedTimestamps)

  /**
   * restarts the Timer with no elapsed time
   */
  def restart = copy(Nil, Nil)

  /**
   * Stop the timer, store the number of elapsed millis and return a String representing the time as hour/minute/second/ms
   * @return the elapsed time as a String
   */
  def stop = copy((getTime - lastTimestamp) :: elapsedTimes, startedTimestamps.drop(1))

  /** add 2 timers together */
  def add(t: HmsTimer[T]) =
    copy((elapsedTimes ++ t.elapsedTimes).filterNot(_ == 0), startedTimestamps ++ t.startedTimestamps)

  /** @return true if this timer has been started */
  def isStarted = startedTimestamps.nonEmpty
  /** @return true if this timer has never been started */
  def neverStarted = !isStarted && elapsedTimes.isEmpty

  def totalMillis =
    if (isStarted) lastTimestamp - firstTimestamp
    else           elapsedTimes.sorted.lastOption.getOrElse(0L)

  private def lastTimestamp = startedTimestamps.sorted.lastOption.getOrElse(0L)
  private def firstTimestamp = startedTimestamps.sorted.headOption.getOrElse(0L)
  /**
   * @return a tuple with the elapsed hours, minutes, seconds and millis
   */
  def hourMinutesSecondsMillis = {
    val hours = totalMillis / 1000 / 3600
    val totalMillis1 = totalMillis - hours * 3600 * 1000
    val minutes = totalMillis1 / 1000 / 60
    val totalMillis2 = totalMillis1 - minutes * 60 * 1000
    val seconds = totalMillis2 / 1000
    val millis = totalMillis2 - seconds * 1000
    (hours, minutes, seconds, millis)
  }

  /**
   * @return a formatted string showing the hours, minutes and seconds
   */
  def hms: String = {
    val (hours, minutes, seconds, millis) = hourMinutesSecondsMillis
    List(hours.toInt.strictlyPositiveOrEmpty("hour"),
         minutes.toInt.strictlyPositiveOrEmpty("minute"),
         seconds.toInt.qty("second")).filter(_.nonEmpty).mkString(" ")
  }

  /**
   * @return a formatted string showing the hours, minutes, seconds and millis
   */
  def time: String = {
    val (_, _, _, millis) = hourMinutesSecondsMillis
    (if (hms != "0 second") hms + ", " else "") +
    millis + " ms"
  }

  /**
   * this method can be overriden for testing
   */
  protected def getTime = Calendar.getInstance.getTime.getTime
}

class SimpleTimer extends HmsTimer[SimpleTimer] {
  def copy(e: List[Long] = Nil, m: List[Long] = Nil) =
    new SimpleTimer {
      override protected val elapsedTimes = e
      override val startedTimestamps = m
    }

  override def toString = hms

  override def equals(a: Any) = a match {
    case s: SimpleTimer => true
    case other          => false
  }

  override def hashCode =
    elapsedTimes.hashCode
}

object SimpleTimer {
  def fromString(s: String) = new SimpleTimer {
    override protected val elapsedTimes = tryOrElse(List(java.lang.Long.parseLong(s)))(Nil)
  }

  def timerFold[T] = new Fold[Id, T, SimpleTimer] {
    type S = SimpleTimer
    val monad = Monad.idMonad

    def start = (new SimpleTimer).start
    def fold: (S, T) => S = (s, t) => s
    def end(s: S) =  s.stop
  }

}
