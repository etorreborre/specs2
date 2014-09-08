package org.specs2
package time

import scala.concurrent.duration.{Duration, MILLISECONDS}

@deprecated(message = "Durations are now implemented with scala.concurrent.duration. Import `scala.concurrent.duration._` instead", since = "3.0")
trait TimeConversions extends DurationConversions {

  implicit class longAsTime(l: Long) {
    def toLong = l

    def seconds = (toLong * 1000).millis
    def second = seconds
    def milliseconds = Duration(toLong, MILLISECONDS)
    def millisecond = milliseconds
    def millis = milliseconds
    def minutes = (toLong * 1000 * 60).millis
    def minute = minutes
    def hours = (toLong * 1000 * 60 * 60).millis
    def hour = hours
    def days = (toLong * 1000 * 60 * 60 * 24).millis
    def day = days
  }

  implicit def intToRichLong(v: Int) = new longAsTime(v.toLong)
}

@deprecated(message = "Durations are now implemented with scala.concurrent.duration. There is no need for conversions between Scala and specs2 durations now", since = "3.0")
trait DurationConversions {
  implicit def concurrentToSpecs2(duration: scala.concurrent.duration.Duration): Duration =
    duration
}

@deprecated(message = "Durations are now implemented with scala.concurrent.duration. There is no need for conversions between Scala and specs2 durations now", since = "3.0")
trait NoDurationConversions extends DurationConversions {
  override def concurrentToSpecs2(duration: scala.concurrent.duration.Duration): Duration =
    super.concurrentToSpecs2(duration)
}

/**
 * This trait can be used to deactivate the time conversions (to avoid conflicts with Akka's conversions for example
 */
@deprecated(message = "Durations are now implemented with scala.concurrent.duration. There is no need to remove specs2 implicits now", since = "3.0")
trait NoTimeConversions extends TimeConversions {
  override def intToRichLong(v: Int) = super.intToRichLong(v)
  override def longAsTime(v: Long)   = super.longAsTime(v)
}

@deprecated(message = "Durations are now implemented with scala.concurrent.duration. There is no need to remove specs2 implicits now", since = "3.0")
object NoTimeConversions extends NoTimeConversions

@deprecated(message = "Durations are now implemented with scala.concurrent.duration. There is no need to remove specs2 implicits now", since = "3.0")
object TimeConversions extends TimeConversions
