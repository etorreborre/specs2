package org.specs2
package mock
package mockito

trait NumberOfTimes {
  /**
   * This implicit definition allows to declare a number of times
   * <code>3.times</code>
   */
  implicit def integerToRange(n: Int): RangeInt = new RangeInt(n)
  case class RangeInt(n: Int) {
    def times = this
  }
}
