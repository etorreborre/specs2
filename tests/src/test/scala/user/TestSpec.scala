package user

import org.specs2._

class TestSpec extends Specification with ScalaCheck { def is = s2"""

 e1 $e1

"""

  def e1 = prop { (n: Int) =>
    n ==== n
  }
}

class TestMutableSpec extends org.specs2.mutable.Specification:

  "e1" >> ok
