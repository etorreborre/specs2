package org.specs2
package time

import java.util.Calendar
import text.Plural._
import control.Exceptions._

/**
 * This trait provides Timer functionalities based on the Java Calendar milliseconds
 */
trait HmsTimer[T <: HmsTimer[T]] {
  /** elapsed time since the last stop */
  val elapsed: Long = 0L
  /** current number of millis when instantiating the object using this Trait */
  val millis: List[Long] = Nil

  def copy(time: Long, m: List[Long]): T
  /**
   * starts the with new elapsed time
   */
  def start = copy(0L, getTime :: millis)

  /**
   * restarts the Timer with no elapsed time
   */
  def restart = copy(0L, Nil)

  /**
   * Stop the timer, store the number of elapsed millis and return a String representing the time as hour/minute/second/ms
   * @return the elapsed time as a String
   */
  def stop = {
    val startMillis = millis.headOption.getOrElse(0L)
    val newStack = if (!millis.isEmpty) millis.drop(1) else millis
    copy(getTime - startMillis, newStack)
  }

  /**
   * @return a tuple with the elapsed hours, minutes, seconds and millis
   */
  def hourMinutesSecondsMillis = {
    val totalMillis = elapsed
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
    var result = ""
    if (hours > 0) { result += hours + " hour".plural(hours) + " " }
    if (minutes > 0) { result += minutes + " minute".plural(minutes) + " " }
    result += (seconds + " second".plural(seconds))
    result
  }

  /**
   * @return a formatted string showing the hours, minutes, seconds and millis
   */
  def time: String = {
    val (hours, minutes, seconds, millis) = hourMinutesSecondsMillis
    (if (hms != "0 second") hms + ", " else "") +
    millis + " ms"
  }

  /**
   * this method can be overriden for testing
   */
  protected def getTime = Calendar.getInstance.getTime.getTime
}
class SimpleTimer extends HmsTimer[SimpleTimer] {
  def copy(e: Long = 0L, m: List[Long] = Nil) = 
    new SimpleTimer {
      override val elapsed = e
      override val millis = m
    }
  def add(t: SimpleTimer) = copy(elapsed + t.elapsed, millis ++ t.millis)
  override def toString = hms

  override def equals(a: Any) = a match {
    case s: SimpleTimer => true
    case other          => false
  }
}

object SimpleTimer {
  def fromString(s: String) = new SimpleTimer {
    override val elapsed = tryOrElse(java.lang.Long.parseLong(s))(0)
  }
}
