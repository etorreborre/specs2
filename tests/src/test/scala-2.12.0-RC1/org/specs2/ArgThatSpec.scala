package org.specs2

import org.specs2.execute.Typecheck._
import org.specs2.execute._
import org.specs2.matcher._
import org.specs2.mock.Mockito

class ArgThatSpec extends Specification with TypecheckMatchers with Mockito { def is = s2"""

 the argThat implicit conversion must be safe $e1

"""

  def e1 = {
    // see #260
    typecheck { """
      object test extends Mockito {
        def beTrueCustom: Matcher[Boolean] = beTrue ^^ {(t: Boolean) => t}
        def someFun(x: Int): (Option[String], Boolean) = (Some("Test"), true)
        someFun(42) must beTrueCustom
      }
    """
    } must failWith(".*required: org.specs2.matcher.Matcher.*Option.String.. Boolean...*")
  }


}
