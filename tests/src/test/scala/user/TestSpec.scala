package user

import org.specs2._

class TestSpec extends Specification with ScalaCheck { def is = s2"""

 e1 $e1

"""

  def e1 = prop { n: Int =>
    n ==== n
  }

}

class TestMutableSpec extends mutable.Specification with ScalaCheck {
  "e1" >> prop { n: Int =>
    n ==== n
  }

}

import org.specs2.concurrent.ExecutionEnv
import org.specs2.mock.Mockito
import scala.concurrent.Future

class ExampleSpec(implicit ee: ExecutionEnv) extends mutable.Specification with Mockito {

  class Test {

    def fn1(a: Int, b: Option[Int] = None): Int =
      a + b.getOrElse(0)

    def fn2(a: Int, b: Option[Int] = None): Future[Int] =
      Future.successful(a + b.getOrElse(0))
  }

  val test = mock[Test]

  test.fn1(anyInt, any[Option[Int]]).returns(3)
  test.fn2(anyInt, any[Option[Int]]).returns(Future.successful(3))

  "sync" should {
    "work with explicit argument" in {
      test.fn1(100, None) === 3
    }
    "work with default argument" in {
      test.fn1(100) === 3
    }
  }
  "async" should {
    "work with explicit argument" in {
      test.fn2(100, None) must ===(3).await
    }
    "work with default argument" in {
      test.fn2(100) must ===(3).await
    }
  }
}
