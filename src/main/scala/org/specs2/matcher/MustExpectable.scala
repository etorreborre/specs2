package org.specs2
package matcher

class MustExpectable[T](t: =>T) extends Expectable[T](t) {
  def aka(alias: String) = new MustExpectable(t) {
	  override protected val desc = Some(alias)
  } 
  def mustNot(m: =>Matcher[T]) = applyMatcher(m.not)
  def must(m: =>Matcher[T]) = applyMatcher(m)
  def must_==(other: =>T) = must(new BeEqualTo(other))
}
