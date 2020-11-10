package org.specs2
package control

import scala.util.Not

/**
 * Syntactic sugar to execute an action a given number of times
 */
trait NumberOfTimes:
  /**
   * This implicit definition allows to declare a number of times
   * `3.times`
   */
  extension (n: Int)(using not: Not[NoNumberOfTimes])
    def times: Times = Times(n)

trait NoNumberOfTimes extends NumberOfTimes:
  given NoNumberOfTimes = ???

object NumberOfTimes extends NumberOfTimes

case class Times(n: Int)
