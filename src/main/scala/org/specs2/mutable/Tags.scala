package org.specs2
package mutable

import specification.TagsFragments._
import specification.Fragment

/**
 * The tags trait allows the creation of Tags fragments in a mutable specification where the tags are defined for the
 * _next_ fragment (or fragments for a section)
 *
 */
trait Tags extends org.specs2.specification.Tags { outer: FragmentsBuilder =>
  /** create a Tag fragment */
  override def tag(names: String*): TaggingFragment = {
    val t = Tag(names:_*)
    addFragments(t)
    t
  }
  /** create a Section fragment */
  override def section(names: String*): TaggingFragment = {
    val t = Section(names:_*)
    addFragments(t)
    t
  }
  /**
   * This implicit allows to add tags and sections _after_ the examples
   */
  implicit def fragmentTaggedAs[T <: Fragment](f: T) = new FragmentTaggedAs(f)
  class FragmentTaggedAs[T <: Fragment](f: T) {
    def tag(names: String*): T = {
      val t = TaggedAs(names:_*)
      addFragments(t)
      f
    }
    def section(names: String*): T = {
      val t = AsSection(names:_*)
      addFragments(t)
      f
    }
  }
}
