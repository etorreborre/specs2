package org.specs2
package guide
package matchers

object TaskMatchers extends UserGuideCard with matcher.TaskMatchers {
  def title = "Scalaz Task"
  def text = s2"""
There are several matchers to check `scalaz.concurrent.Task` instances. To use them, you need to include the `specs2-scalaz` jar in your dependencies and mix in the `TaskMatchers` trait to your specification. 

Then you can use:

 * `returnOk` to check if a `Task` completes successfully
 * `returnBefore(duration)` to check if a `Task` completes successfully before the specified duration
 * `returnValue(matcher)` to check if a `Task` successfully returns a value that satisfies the matcher
 * `returnValue(function: A => AsResult[B])` to check if a `Task` completes successfully with a value `a`, where `function(a)` returns a successful [Result](org.specs.guide.AsResultTypeclass.html). This can be used to do conditional assertions that depend on which value is returned by your `Task`
 * `failWith[ExceptionType]` to check if a `Task` fails with an Exception of the given type 
 
#### Task execution
 Please note that all the matchers above will run the `Task` for you, you should not call `.run`/`.unsafePerformSync` on them in your tests.
  """
}
