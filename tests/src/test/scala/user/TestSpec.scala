package user

import org.specs2._
import org.specs2.matcher.JsonMatchers

class TestSpec extends Specification with ScalaCheck with JsonMatchers { def is = s2"""

 e1 $e1

"""

  def e1 = {
      """{ "b" : { "a" : 2, "c" : null } }""" must /("b" -> /("a" -> 2))
  }

}


class TestMutableSpec extends mutable.Specification {

}
