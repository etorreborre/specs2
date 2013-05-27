package examples

import org.specs2._

class TestSpec extends SpecificationWithJUnit { def is = s2"""
This is a simple, hierarchical specification
  if things are indented
        in the console                   $ok
        as well as the HTML report       $ko
      just as expected
        by me                            $todo
        or maybe someone else            $ok
    or if there's only text, indentation
    with more text
      should also work                   $ok
      and not quote lines as code        $ok

   ${link(new Test2Spec)}
"""

}

class Test2Spec extends SpecificationWithJUnit { def is =
"This is a simple, hierarchial specification" ^
  "If things are indented1"                    ^
    "by me1"                                   ! ok^
    "by me2"                                   ! ok^endp^
  "If things are indented2"                    ^
    "by me3"                                   ! ok^
    "by me4"                                   ! ok

}
