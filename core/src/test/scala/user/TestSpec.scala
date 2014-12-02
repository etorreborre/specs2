package user

import org.specs2.Specification

class Test1Spec extends Specification { def is = s2"""

  e $ok
  e2 $ok
"""
}

class Test2Spec extends org.specs2.mutable.Specification {
  skipAllIf(true)

  "ex1" in ok
  "ex2" in ok

}

class Test3Spec extends Specification { def is = skipAll ^
  p^
  "e1" ! ok ^ br^
  "e2" ! ok

}
