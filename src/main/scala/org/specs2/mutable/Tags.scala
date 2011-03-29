package org.specs2
package mutable

import specification.TagsFragments._

/**
 * The tags trait allows the creation of Tags fragments in a mutable specification where the tags are defined for the
 * _next_ fragment (or fragments for a section)
 */
trait Tags extends org.specs2.specification.Tags { this: FragmentsBuilder =>
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
}
