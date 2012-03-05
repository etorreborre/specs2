package examples

import org.specs2.mutable.Specification
import org.specs2.mock.Mockito
import org.specs2.matcher._
import org.hamcrest.{Description, TypeSafeMatcher}

class TestSpec extends Specification with Mockito {

  trait ToMock {
    def fn(arg1: =>String): Any
  }
  val m = mock[ToMock]
//  def call[A](arg: A) = be_==(arg) ^^ { (f: Function0[A]) => f() }

//  implicit def matcherFunction0ToValue[A](ma: Matcher[A]): A]] = ma ^^ { (f: Function0[A]) => f() }

//  def argThat2[T, U <: T](m: org.specs2.matcher.Matcher[U]): T =
//    org.mockito.Matchers.argThat(new examples.HamcrestMatcherAdapter2(m))

  "try that" >> {
    m.fn("string")
    there was one(m).fn(argThat(be_===("string")))
  }
}

/**
 * Adapter class to use specs2 matchers as Hamcrest matchers
 */
//case class HamcrestMatcherAdapter2[T](m: Matcher[T]) extends TypeSafeMatcher[(=>T] {
//  /** this variable is necessary to store the result of a match */
//  private var message = ""
//
//  def matchesSafely(item: T): Boolean = {
//    m.apply(Expectable(item)) match {
//      case MatchFailure(_, m, _, _) => message = m; false
//      case _ => true
//    }
//
//  }
//  def describeTo(description: Description) = {
//    description.appendText(message)
//  }
//}
