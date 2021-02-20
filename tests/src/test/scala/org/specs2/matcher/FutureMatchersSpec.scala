package org.specs2
package matcher

import execute.*
import fp.*, syntax.*
import specification.core.Env
import concurrent.*
import scala.concurrent.*
import duration.*
import runner.*
import main.Arguments

class FutureMatchersSpec extends Specification with ResultMatchers with specification.Retries:

  lazy val env = Env(Arguments("threadsnb 4"))
  lazy val timeFactor = env.arguments.execute.timeFactor
  lazy val sleepTime = 50 * timeFactor.toLong
  given ee: ExecutionEnv = env.executionEnv

  class MyTimeout extends TimeoutException

  def is = section("travis") ^ sequential ^ s2"""

 In this specification `Future` means `scala.concurrent.Future`

 Any `Matcher[T]` can be transformed into a `Matcher[Future[T]]` with the `await` method
 test ${ Future.apply(1) `must` be_>(0).await }

 with a retries number and timeout
 ${ Future { sleep(sleepTime); 1 } `must` be_>(0).await(retries = 3, timeout = 100.millis) }
 ${ (Future { sleep(sleepTime * 3); 1 } `must` be_>(0).await(retries = 4, timeout = 10.millis)) `returns` "Timeout" }

 with a retries number only
 ${ Future { sleep(sleepTime); 1 } `must` be_>(0).retryAwait(2) }

 with a timeout only
 ${ Future { sleep(sleepTime); 1 } `must` be_>(0).awaitFor(200.millis) }

 timeout applies only to `TimeoutException` itself, not subclasses
 ${ (Future { throw new TimeoutException } `must` throwA[TimeoutException]().await) `returns` "Timeout" }
 ${ Future { throw new MyTimeout } `must` throwA[MyTimeout]().await }

 A `Future` returning a `Matcher[T]` can be transformed into a `Result`
 ${ Future(1 === 1).await }

 A `throwA[T]` matcher can be used to match a failed future with the `await` method
 ${ Future.failed[Int](new RuntimeException) `must` throwA[RuntimeException]().await }
 ${ { Future.failed[Int](new RuntimeException) `must` be_===(1).await } `must` throwA[RuntimeException] }

 A Future expression throwing an exception must not be matched
 ${ ({ throw new Exception("boom"); Future(1) } `must` throwAn[Exception]().await) `must` throwAn[Exception] }

 In a mutable spec with a negated matcher $e1
 In a mutable spec with a negated matcher - and a timeout $e2

 A Future should be retried the specified number of times in case of a timeout $e4
 A Future should not be called more than the expected number of times $e5
""" ^ step(env.shutdown())

  def e1 =
    case class Spec1() extends FutureMatchers with MustThrownExpectations:
      def result = Future(true) `must` beFalse.awaitFor(1 second)
    Spec1().result `must` throwA[FailureException]

  def e2 =
    case class Spec1() extends FutureMatchers with MustThrownExpectations:
      def result = Future { Thread.sleep(2000); 10} `must` beGreaterThan(100).awaitFor(1 second)
    Spec1().result `must` throwA[FailureException]

  def e4 =
    val retries = 2
    var times = 0
    val duration = 50l
    def future = Future {
      times += 1
      if retries != times then
        try Thread.sleep(duration * 4) catch { case _: Throwable => 0 }
      else
        0
    }
    future `must` be_==(0).await(retries, duration.millis)

  def e5 =
    val retries = 0
    var times = 0
    def future = Future {
      times += 1
      0
    }
    future `must` be_==(0).retryAwait(retries)
    times `must` be_==(1)

  def sleep(millis: Long): Unit = try
    Thread.sleep(millis)
  catch { case _: InterruptedException => () }
