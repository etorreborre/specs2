package org.specs2
package matcher

import org.specs2.matcher.describe.Diffable

/**
 * This kind of expectable can be followed by the verb should to apply a matcher:
 * 
 * `1 should beEqualTo(1)`
 * 
 * For convenience, several shouldMatcher methods have also been defined as shortcuts to equivalent:
 * 
 * `a should matcher`
 */
class ShouldExpectable[T] private[specs2] (tm: () => T) extends Expectable[T](tm){
  def should(m: =>Matcher[T])      = applyMatcher(m)
  def shouldEqual(other: =>Any)    = applyMatcher[Any](new BeEqualTo(other))
  def shouldNotEqual(other: =>Any) = applyMatcher[Any](new BeEqualTo(other).not)
  def should_==(other: =>Any)      = applyMatcher[Any](new BeEqualTo(other))
  def should_===(other: =>T)(implicit di: Diffable[T])  = applyMatcher(new BeTypedEqualTo2(other))
  def should_!=(other: =>Any)      = applyMatcher[Any](new BeEqualTo(other).not)
}
object ShouldExpectable {
  def apply[T](t: =>T) = new ShouldExpectable(() => t)
}
