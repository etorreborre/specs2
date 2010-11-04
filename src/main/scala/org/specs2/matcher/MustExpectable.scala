package org.specs2
package matcher

/**
 * This kind of expectable can be followed by the verb must to apply a matcher:
 * 
 * `1 must beEqualTo(1)`
 * 
 * For convenience, several mustMatcher methods have also been defined as shortcuts to equivalent:
 * 
 * `a must matcher`
 */
class MustExpectable[T] private (tm: () => T) extends Expectable[T](tm) { outer =>
  /** @return this expectable with an alias description */
  override def aka(alias: String): MustExpectable[T] = new MustExpectable(tm) {
	  override protected val desc = Some(alias)
  } 
  def mustNot(m: =>Matcher[T]) = applyMatcher(m.not)
  def must(m: =>Matcher[T]) = applyMatcher(m)
  def must_==(other: =>T) = applyMatcher(new BeEqualTo(other))
}
object MustExpectable {
  def apply[T](t: =>T) = new MustExpectable(() => t)
}

