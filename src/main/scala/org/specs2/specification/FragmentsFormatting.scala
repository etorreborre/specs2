package org.specs2
package specification

import TagsFragments.TaggingFragment

trait FragmentsFormatting {
  /** format a list of fragments according to their formatting tags */
  def formatFragments: Fragments => Fragments
}

trait DefaultFragmentsFormatting extends FragmentsFormatting with TagsAssociation {

  def formatFragments: Fragments => Fragments = (fs: Fragments) => {
    val tagged = tagFragments(fs.fragments).map {
      case (t: Text, tag)    => t.copy(text = formattedStringFor(tag)(t.text))
      case (e: Example, tag) => e.formatWith(formattedStringFor(tag)(e.desc))
      case (other, _)        => other
    }
    Fragments.create(tagged:_*)
  }

  private def formattedStringFor(tag: TaggingFragment) = (formatted: FormattedString) =>
    formatted.formatWithTagNames(tag.names)
}

object DefaultFragmentsFormatting extends DefaultFragmentsFormatting