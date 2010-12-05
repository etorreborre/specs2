package org.specs2
package matcher

/**
 * This kind of expectable can be followed by the verb should to apply a matcher:
 * 
 * `1 should beEqualTo(1)`
 * 
 * For convenience, several shouldMatcher methods have also been defined as shortcuts to equivalent:
 * 
 * `a should matcher`
 */
class ShouldExpectable[T] private (tm: () => T) extends Expectable[T](tm){
  override def aka: ShouldExpectable[T] = new ShouldExpectable(tm) {
    override protected val desc = Some(tm().toString)
  } 
  /** @return this expectable with an alias description */
  override def aka(alias: String): ShouldExpectable[T] = new ShouldExpectable(tm) {
	  override protected val desc = Some(alias)
  } 
  def should(m: =>Matcher[T]) = applyMatcher(m)
  def shouldNot(m: =>Matcher[T]) = applyMatcher(m.not)
  def should_==(other: =>T) = applyMatcher(new BeEqualTo(other))
}
object ShouldExpectable {
  def apply[T](t: =>T) = new ShouldExpectable(() => t)
}
