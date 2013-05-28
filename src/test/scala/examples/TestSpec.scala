package examples

import org.specs2._
import specification._
import specification.Text

class TestSpec extends SpecificationWithJUnit { def is = s2""" ${section("specs2.format.markdown")}
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
      and not quote lines as code        $ok  ${section("specs2.format.markdown")}

   ${link(new Test2Spec)}
"""

  override def interpolatedArguments = args.report(noindent = false, flow = false)
  override def map(fs: =>Fragments) = fs.map {
    case Text(text) => Text(text.withMarkdown.withFlow)
    case other    => other
  }
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
