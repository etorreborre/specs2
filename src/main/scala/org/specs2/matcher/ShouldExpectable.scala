package org.specs2
package matcher

class ShouldExpectable[T](t: =>T) extends Expectable[T](t){
  def aka(alias: String) = new ShouldExpectable(t) {
	override protected val desc = Some(alias)
  } 
  def should(m: =>Matcher[T]) = applyMatcher(m)
  def shouldNot(m: =>Matcher[T]) = applyMatcher(m.not)
  def should_==(other: =>T) = should(new BeEqualToMatcher(other))
}
