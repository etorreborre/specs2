package org.specs2
package control

import mutable.Specification
import Functions._

class FunctionsSpec extends Specification {
  "a strict function can be transformed into a byname one" >> {
    var result= 0
    val f = (i: Int) => { result = 1 }
    toByNameFunction1(f){ result = 2; 3 }
    result === 1
  }
  "a byname function can be transformed into a strict one" >> {
    var result= 0
    def f(i: =>Int) = 1
    toStrictFunction1(f){ result = 2; 3 }
    result === 2
  }
}
