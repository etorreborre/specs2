package org.specs2
package mock

import matcher._
import junit.framework.AssertionFailedError
import org.specs2.fp.Monad

class MockitoSpec extends Spec with Mockito with ResultMatchers {  def is = s2"""

  Contexts
  ========

  The Mockito trait is reusable in other contexts
     in JUnit $junit1

 Issues
 ======

 The following mockito issues are fixed
   #584 NPE when setting expectations twice on a mockito mock $bug1
   #603 classcast exception with smart return values $bug2
   #679 any[T] matcher must work like org.mockito.ArgumentMatchers.any $bug3

"""

  def junit1 = {
    val s = new Mockito with JUnitExpectations {
      val list = mock[java.util.List[String]]
      def test = {
        list.add("one")
        there was one(list).add("two")
        1 must_== 1 // to check if the previous expectation really fails
      }
    }
    s.test must throwAn[AssertionFailedError]
  }

  def bug1 = {
    trait Mockable { def method(list: List[String]): String }
    lazy val m = mock[Mockable]

    m.method(===(List[String]())).returns("Hello1")
    m.method(===(List[String]("2"))).returns("Hello2")
    m.method(List()) === "Hello1"
    m.method(List("2")) === "Hello2"
  }

  def bug2 =  {
    class MyDAO[M[_] : Monad] {
      def test: M[Int] = Monad[M].pure(0)
    }

    val m: MyDAO[Option] = mock[MyDAO[Option]].smart
    m.test returns Option(1)

    m.test must beSome(1)
  }

  def bug3 =  {
    class Test {
      def fn1(a: Int, b: Option[Int] = None): Int =
        a + b.getOrElse(0)
    }
    val test = mock[Test]

    test.fn1(anyInt, any[Option[Int]]).returns(3)
    test.fn1(100) ==== 3
  }
}
