package org.specs2
package control

import Functions._
import matcher.ExpectationsDescription._

class FunctionsSpec extends Spec { def is = s2"""

  a byname function can be transformed into a strict one  ${
    def byNameFunction(u: =>Unit) {}
    var parameter = "not evaluated"
    toStrictFunction1(byNameFunction){ parameter = "evaluated" }

    "The byname function has become a strict one" <==> (parameter must ===("evaluated"))
  }

  functions can be or-ed with ||  ${
    val f1: String => Boolean = (_:String).length < 3
    val f2: String => Boolean = (_:String).length < 5

    (f1 || f2)("abcdefg") must beFalse
    (f1 || f2)("abc")     must beTrue
    (f1 || f2)("abcd")    must beTrue
    (f2 || f1)("ab")      must beTrue
  }

  functions can be and-ed with &&" ${
    val f1: String => Boolean = (_:String).length < 3
    val f2: String => Boolean = (_:String).length < 5

    (f1 && f2)("abcdefg") must beFalse
    (f1 && f2)("abc")     must beFalse
    (f1 && f2)("abcd")    must beFalse
    (f2 && f1)("ab")      must beTrue
  }

  functions can be negated with !" ${
    val f1: String => Boolean = (_:String).length < 3

    (!f1)("abcdefg") must beTrue
    (!f1)("ab")      must beFalse
  }

  """
}

