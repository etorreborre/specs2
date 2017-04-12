package org.specs2
package control

/**
 * Syntactic sugar to execute an action a given number of times
 */
trait NumberOfTimes {
  /** 
   * This implicit definition allows to declare a number of times
   * `3.times`
   */
  implicit def timesFor(n: Int): Times = new Times(n)
}

trait NoNumberOfTimes extends NumberOfTimes {
  override def timesFor(n: Int): Times = super.timesFor(n)
}

case class Times(n: Int) {
  def times = this
}

object NumberOfTimes extends NumberOfTimes