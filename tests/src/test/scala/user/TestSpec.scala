package user

import org.specs2._
import org.specs2.data.AlwaysTag

class TestSpec extends Specification with ScalaCheck { def is = s2"""

 child1 $${link(Child1)}
 child2 $${link(Child2)}
 child3 $${link(Child3)}

"""

  def e1 = {
    ok
  }

}


class TestMutableSpec extends mutable.Specification {
}


class ParentIntegrationSpec extends Specification { def is = s2"""
  Integration Tests
  ${"c2" ~ Child2} ${tag("c2")}
  ${"c1" ~ Child1} ${tag("c1")}
  myTest ${myTest} ${tag(AlwaysTag)}
  """
  def myTest = ok
}
object Child2 extends Specification { def is = s2"""
            Child2 ${child2}
  """
  def child2 = ok
}
object Child1 extends Specification { def is = s2"""
            Child1 ${child1}
  """
  def child1 = ko
}
