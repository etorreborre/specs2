package org.specs2
package specification
import TagsFragments._

/**
 * The tags trait allows the creation of Tags fragments in a specification
 */
trait Tags {
  /** create a TaggedAs fragment */
  def tag(names: String*) = TaggedAs(names:_*)
  /** create a AsSection fragment */
  def section(names: String*) = AsSection(names:_*)
}
object Tags extends Tags