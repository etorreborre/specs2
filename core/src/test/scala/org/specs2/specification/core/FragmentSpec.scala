package org.specs2.specification.core

import org.specs2.Specification
import org.specs2.matcher.ThrownExpectations

class FragmentSpec extends Specification with ThrownExpectations { def is = s2"""

  An example is anything which can be executed but has some text to display,
   so it is not a step or an action or a specification reference $isExample

"""

  def isExample = {
    List(step(()), action(()), br, t(1), bt(1), tag("n"), link(new FragmentSpec)) must not(contain((f: Fragment) => Fragment.isExample(f)))
    List("ex" ! ok, Fragment(Code("script"), Execution.result(ok))) must contain((f: Fragment) => Fragment.isExample(f)).forall
  }


}
