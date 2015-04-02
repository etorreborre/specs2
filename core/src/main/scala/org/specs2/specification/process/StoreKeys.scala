package org.specs2
package specification
package process

import execute.Result
import org.specs2.io.Key
import time.SimpleTimer
import scala.util.Try
import scalaz.syntax.traverse._
import scalaz.std.list._
import scalaz.std.option._

object StoreKeys {
  def resolve[A](key: Key[A]): String =
    (key match {
      case SpecificationStatsKey(name) => name
      case SpecificationResultKey(name, description) => name+"-"+description.hashCode
    }) + ".stats"

  def encode[A](key: Key[A], value: A): String =
    key match {
      case SpecificationStatsKey(_) =>
        val s: Stats = value
        statsToString(s)+
          s.trend.map(t => "\n"+statsToString(t)).getOrElse("")

      case SpecificationResultKey(_, _) =>
        val r: Result = value
        statsToString(Stats(r))
    }

  def decode[A](key: Key[A], data: String): Option[A] =
    key match {
      case SpecificationStatsKey(_) =>
        val lines = data.split("\n").toList
        lines.map(statsFromString).sequence.map { stats =>
          if (stats.size == 2) stats(0).copy(trend = Some(stats(1)))
          else stats(0)
        }

      case SpecificationResultKey(_, _) =>
        statsFromString(data).map(_.result)

    }


  private def statsToString(s: Stats) = {
    import s._
    s"specs=$specs,examples=$examples,successes=$successes,expectations=$expectations,failures=$failures,errors=$errors,pending=$pending,skipped=$skipped,time=${timer.totalMillis}"
  }

  private def statsFromString(s: String): Option[Stats] = Try {
    s.split(",").map(_.split("=")(1)).toList match {
      case List(specs,examples,successes,expectations,failures,errors,pending,skipped,time) =>
        Stats(specs.toInt, examples.toInt, successes.toInt, expectations.toInt, failures.toInt, errors.toInt, pending.toInt, skipped.toInt, trend = None, SimpleTimer.fromString(time))
    }
  }.toOption

}
