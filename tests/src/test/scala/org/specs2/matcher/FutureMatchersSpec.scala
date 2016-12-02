package org.specs2
package matcher

import execute._
import specification.core.Env
import scala.concurrent._
import duration._
import runner._
import control._

class FutureMatchersSpec(env: Env) extends Specification with ResultMatchers with Retries {
 val timeFactor = env.arguments.execute.timeFactor
 val sleepTime = 50 * timeFactor.toLong
 implicit val ee = env.executionEnv
 implicit val ec = env.executionContext
 class MyTimeout extends TimeoutException

 def is = section("travis") ^ s2"""

 In this specification `Future` means `scala.concurrent.Future`

 Any `Matcher[T]` can be transformed into a `Matcher[Future[T]]` with the `await` method
 test ${ Future.apply(1) must be_>(0).await }

 with a retries number and timeout
 ${ Future { Thread.sleep(sleepTime); 1 } must be_>(0).await(retries = 3, timeout = 100.millis) }
 ${ (Future { Thread.sleep(sleepTime * 3); 1 } must be_>(0).await(retries = 4, timeout = 10.millis)) returns "Timeout" }

 with a retries number only
 ${ Future { Thread.sleep(sleepTime); 1 } must be_>(0).retryAwait(2) }

 with a timeout only
 ${ Future { Thread.sleep(sleepTime); 1 } must be_>(0).awaitFor(200.millis) }

 timeout applies only to `TimeoutException` itself, not subclasses
 ${ (Future { throw new TimeoutException } must throwA[TimeoutException].await) returns "Timeout" }
 ${ Future { throw new MyTimeout } must throwA[MyTimeout].await }

 A `Future` returning a `Matcher[T]` can be transformed into a `Result`
 ${ Future(1 === 1).await }

 A `throwA[T]` matcher can be used to match a failed future with the `await` method
 ${ Future.failed[Int](new RuntimeException) must throwA[RuntimeException].await }
 ${ { Future.failed[Int](new RuntimeException) must be_===(1).await } must throwA[RuntimeException] }

 A Future expression throwing an exception must not be matched
 ${ ({ throw new Exception("boom"); Future(1) } must throwAn[Exception].await) must throwAn[Exception] }

 In a mutable spec with a negated matcher $e1
 In a mutable spec with a scope $e2

"""

  def e1 = {
    val thrown = new FutureMatchers with MustThrownExpectations {
      def result = Future(true) must beFalse.awaitFor(1 second)
    }
   thrown.result must throwA[FailureException]
  }

  def e2 = {
    val thrown = new mutable.Specification with FutureMatchers {
      "timeout ko" in new Scope {
        Future {
          try Thread.sleep(100) catch { case _: Throwable => () }
          1 must_== 2
        }.awaitFor(50.millis)
      }
    }

    ClassRunner.report(env)(thrown).runOption.get.failures === 1
  }
}
