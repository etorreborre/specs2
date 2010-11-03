package org.specs2
package matcher

class MustExpectable[T](tm: () => T) extends Expectable[T](tm) { outer =>
  def aka = new MustExpectable(tm) {
    override protected val desc = Some(tm().toString)
  } 
  def aka(alias: String) = new MustExpectable(tm) {
	  override protected val desc = Some(alias)
  } 
  def mustNot(m: =>Matcher[T]) = applyMatcher(m.not)
  def must(m: =>Matcher[T]) = applyMatcher(m)
  def must_==(other: =>T) = must(new BeEqualTo(other))
}
