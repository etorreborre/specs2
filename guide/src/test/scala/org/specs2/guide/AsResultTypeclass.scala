package org.specs2
package guide

object AsResultTypeclass extends UserGuidePage { def is = "AsResult typeclass".title ^ s2"""
There are many ways to define expectations in $specs2:

  * Boolean (`true, false`)
  * Standard result (`success, failure`)
  * Matcher result (`1 must_== 1`)
  * Scalacheck property
  * DataTable
  * Forms

All of these types implement the `org.specs2.execute.AsResult` typeclass, meaning that they can be transformed into a `Result`:${snippet{
trait AsResult[T] {
  def asResult(t: =>T): Result
}
}}

This gives some flexibility in integrating any kind of custom definition of a "result" into $specs2 and this is why you find this typeclass as a requirement to build examples or to declare contexts.
You can take advantage of this type class by defining your own kind of result and providing a typeclass instance for it:${snippet{
// 8<----
trait AsResult[T] {
  def asResult(t: =>T): Result
}
// 8<----
// A new type of results for cluster execution
trait ClusterExecution {
  def succeeded: Boolean
  def errorMessage: String
}

object ClusterExecution {
  implicit def clusterExecutionAsResult: AsResult[ClusterExecution] =
    new AsResult[ClusterExecution] {
      def asResult(t: =>ClusterExecution): Result = {
        try {
          val result = t
          if (result.succeeded) Success()
          else                  Failure(t.errorMessage)
        } catch { case e: Throwable => Error(e) }
      }
   }
}
}}

#### Decorated results

You can also embed custom data in a special kind of `Result`, a `DecoratedResult`:${snippet {
case class DecoratedResult[+T](decorator: T, result: Result) extends Result(result.message, result.expected)
}}

A `DecoratedResult[T]` decorates an ordinary result with an additional value of type `T`. If you want to take advantage of this custom value in your reports you will need to build a custom `org.specs2.reporter.Printer`, probably extending an existing one.

$AndIfYouWantToKnowMore

 - mark any object having an `AsResult` instance as ${"\"pending until fixed\"" ~/ PendingUntilFixedExamples}

$vid
"""
  class Result(val message: String, val expected: String)
  case class Success() extends Result("ok", "")
  case class Failure(override val message: String) extends Result(message, "")
  case class Error(e: Throwable) extends Result(e.getMessage, "")
}

