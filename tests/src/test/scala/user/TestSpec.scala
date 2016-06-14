package user

import org.specs2._

class TestSpec extends Specification with ScalaCheck { def is = s2"""

"""

  def e1 = {
    ok
  }

}


class TestMutableSpec extends mutable.Specification {
  isolated

  "understanding isolation quirks" >> {
    val x = new scala.collection.mutable.ArrayBuffer[Int]
    "we increment i " >> {
      x += 1
      "the count should be up by one" >> {
        x.size must_== 1
      }

      "and more nested tests" >> {
        "more" >> {
          1 must_== 1
        }
      }
    }

    "i should be zero " >> {
      x must beEmpty
    }
  }
}
