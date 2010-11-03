package org.specs2
package matcher

class ShouldExpectable[T](tm: () => T) extends Expectable[T](tm){
  def aka = new ShouldExpectable(tm) {
    override protected val desc = Some(tm().toString)
  } 
  def aka(alias: String) = new ShouldExpectable(tm) {
	  override protected val desc = Some(alias)
  } 
  def should(m: =>Matcher[T]) = applyMatcher(m)
  def shouldNot(m: =>Matcher[T]) = applyMatcher(m.not)
  def should_==(other: =>T) = should(new BeEqualTo(other))
}
