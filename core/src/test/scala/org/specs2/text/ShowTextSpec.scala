package org.specs2
package text

class ShowTextSpec extends Specification { def is = s2"""

 Show x instances can be used to specify formatting functions for tuples $showTuple

"""

  def showTuple = {
    val showInt    = (n: Int)    => (n + 1).toString
    val showDouble = (d: Double) => (d + 2).toString
    val showLong   = (l: Long)   => (l + 3).toString

    ShowText.show1(showInt).show2(showDouble).show3(showLong).show(1, 3.0, 10L) ====
      (("2", "5.0", "13"))
  }

}
