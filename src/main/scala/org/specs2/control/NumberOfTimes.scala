package org.specs2
package control

trait NumberOfTimes {
  /** 
   * This implicit definition allows to declare a number of times
   * `3.times`
   */
  implicit def timesFor(n: Int): Times = new Times(n)
}

case class Times(n: Int) {
  def times = this
}

object NumberOfTimes extends NumberOfTimes