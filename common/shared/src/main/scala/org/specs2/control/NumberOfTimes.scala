package org.specs2
package control

import scala.util.NotGiven

/** Syntactic sugar to execute an action a given number of times
  */
trait NumberOfTimes:
  /** This implicit definition allows to declare a number of times `3.times`
    */
  extension (n: Int)(using not: NotGiven[NoNumberOfTimes]) def times: Times = Times(n)

trait NoNumberOfTimes extends NumberOfTimes:
  given NoNumberOfTimes = ???

object NumberOfTimes extends NumberOfTimes

case class Times(n: Int)
