package org.specs2
package specification

import org.specs2.specification.TagsFragments.{TaggedAs, TaggingFragment}
import FormattingFragments._

/**
 * This trait post-process fragments.
 *
 * The default implementation looks for tags and sections to mark text and examples as
 * "markdown"
 */
trait FragmentsFormatting {
  /** format a list of fragments according to their formatting tags */
  def formatFragments: Fragments => Fragments
}

trait DefaultFragmentsFormatting extends FragmentsFormatting with TagsAssociation {

  def formatFragments: Fragments => Fragments = (fs: Fragments) => {
    val taggedFragments = tagFragments(fs.compactTags.fragments)
    val tagged = taggedFragments.flatMap {
      case (t: Text, tag)      => Seq(t.copy(formattedStringFor(tag)(t.text)))
      case (e: Example, tag)   => Seq(e.formatWith(formattedStringFor(tag)(e.desc)))
      case (f, _)              => Seq(f)
    }
    Fragments.create(tagged:_*)
  }

  private def formattedStringFor[F <: FormattedString](tag: TaggingFragment) = (formatted: F) =>
    formatted.formatWithTagNames(tag.names)
}

object DefaultFragmentsFormatting extends DefaultFragmentsFormatting