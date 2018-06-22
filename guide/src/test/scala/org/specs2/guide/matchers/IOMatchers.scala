package org.specs2
package guide
package matchers

object IOMatchers extends UserGuideCard {
  def title = "IO Matchers"
  def text = s2"""
  There are some basic matchers to check `cats.effect.IO` instances:

 * `returnOk` to check if an `IO` completes successfully
 * `returnBefore(duration)` to check if an `IO` completes successfully before the specified duration
 * `returnValue(matcher)` to check if an `IO` successfully returns a value that satisfies the matcher
 * `returnValue(function: A => AsResult[B])` to check if an `IO` completes successfully with a value `a`,
 where `function(a)` returns a successful [Result](org.specs.guide.AsResultTypeclass.html).
 This can be used to do conditional assertions that depend on which value is returned by your `IO`

"""
}
