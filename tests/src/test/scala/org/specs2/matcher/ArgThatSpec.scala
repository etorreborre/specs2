package org.specs2.matcher

import org.specs2.Specification
import org.specs2.execute.Typecheck._

class ArgThatSpec extends Specification with TypecheckMatchers { def is = s2"""

 the argThat implicit conversion must be safe $e1

"""

  def e1 = {
    // see #260
    typecheck { """
      object test {
        def beTrueCustom: Matcher[Boolean] = beTrue ^^ {(t: Boolean) => t}
        def someFun(x: Int): (Option[String], Boolean) = (Some("Test"), true)
        someFun(42) must beTrueCustom
      }
    """
    } must failWith(".*required: org.specs2.matcher.Matcher.*Option.String.. Boolean...*")
  }


}
