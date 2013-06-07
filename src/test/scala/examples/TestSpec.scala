package examples

import org.specs2._
import org.specs2.matcher.Matcher

class TestSpec extends SpecificationWithJUnit { def is = s2""" ${formatSection(verbatim=false)}
This is a simple, hierarchical specification
  If things are indented
    and examples are nested
      this should be reflected
        in the console                   $ok
        as well as the HTML report       $ok
      just as expected
        by me                            $ok
        or maybe someone else            $todo
    or if there's only text, indentation
    possibly on multiple following lines
      should also work                   $ok
      and not quote lines as code        $ok

""" ^ link(new Test2Spec)

  def startWith[T](fromIndex: Int, ts: T*): Matcher[Traversable[T]] =
    startWith(ts:_*) ^^ ((_:Traversable[T]).drop(fromIndex))

  def startWith[T](ts: T*): Matcher[Traversable[T]] =
    contain(allOf(ts:_*)).exactly ^^ ((_:Traversable[T]).take(ts.size))

}

class Test2Spec extends SpecificationWithJUnit { def is = "Test 2 Spec".title ^
"This is a simple specification2"              ^p^
  "If things are indented1"                    ^
  "a bit more"                                 ^
    "by me1"                                   ! ok^
      "by me2"                                 ! ok^
    "If things are indented2"                  ^
      "by me3"                                 ! ok^
      "by me4"                                 ! ok

}
