package org.specs2
package specification

import TagsFragments.TaggingFragment
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
    val tagged = tagFragments(fs.fragments).flatMap {
      case (t: Text, tag)    => {
        val t1 = t.copy(text = formattedStringFor(tag)(t.text))
        Seq(t1)
      }
      case (e: Example, tag) => {
        val e1 = e.formatWith(formattedStringFor(tag)(e.desc))
        Seq(e1)
      }
      case (s: SpecStart, tag) => Seq(s)
      case (f, _)              => Seq(f)
    }
    Fragments.create(tagged:_*)
  }

  private def formattedStringFor[F <: FormattedString](tag: TaggingFragment) = (formatted: F) =>
    formatted.formatWithTagNames(tag.names)
}

object DefaultFragmentsFormatting extends DefaultFragmentsFormatting