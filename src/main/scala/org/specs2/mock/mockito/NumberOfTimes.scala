package org.specs2
package mock
package mockito


trait NumberOfTimes {
  /** 
   * This implicit definition allows to declare a number of times
   * `3.times`
   */
  implicit def integerToRange(n: Int): RangeInt = new RangeInt(n)
  case class RangeInt(n: Int) { 
    def times = this 
  }
}
