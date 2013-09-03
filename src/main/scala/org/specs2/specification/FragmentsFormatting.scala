package org.specs2
package specification

import TagsFragments.TaggingFragment
import FormattingFragments._

/**
 * This trait post-process fragments.
 *
 * The default implementation looks for tags and sections to mark text and examples as
 * "flowing" (no automatic indentation) or "markdown"
 */
trait FragmentsFormatting {
  /** format a list of fragments according to their formatting tags */
  def formatFragments: Fragments => Fragments
}

trait DefaultFragmentsFormatting extends FragmentsFormatting with TagsAssociation {

  def formatFragments: Fragments => Fragments = (fs: Fragments) => {
    val taggedFragments = tagFragments(fs.compactTags.fragments)
    val tagged = taggedFragments.flatMap {
      case (t: Text, tag)    => {
        val t1 = t.copy(formattedStringFor(tag)(t.text))
        if (t1.flow) Seq(t1) else Seq(t1, br)
      }
      case (e: Example, tag) => {
        val e1 = e.formatWith(formattedStringFor(tag)(e.desc))
        if (e1.desc.flow) Seq(e1) else Seq(e1, br)
      }
      case (s: SpecStart, tag) => if (Formatting().fromTagNames(tag.names).flow) Seq(s) else Seq(s, br, br)
      case (f, _)              => Seq(f)
    }
    Fragments.create(tagged:_*)
  }

  private def formattedStringFor[F <: FormattedString](tag: TaggingFragment) = (formatted: F) =>
    formatted.formatWithTagNames(tag.names)
}

object DefaultFragmentsFormatting extends DefaultFragmentsFormatting