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
      case (Text(formatted), tag)            => Text(formattedStringFor(tag)(formatted))
      case (e @ Example(formatted, _), tag)  => e.formatWith(formattedStringFor(tag)(formatted))
      case (other, _)                        => other
    }
    Fragments.create(tagged:_*)
  }

  private def formattedStringFor(tag: TaggingFragment) = (formatted: FormattedString) =>
    formatted.copy(formatting = formatted.formatting.copy(flow = isFlow(tag, formatted.flow), markdown = isMarkdown(tag, formatted.formatting.markdown)))

  private def isFlow(tag: TaggingFragment, currentValue: Boolean) =
    if (tag.isEmpty) currentValue
    else tag.names.exists(_ == FormattingTags.flowSection.names.head) && !tag.names.exists(_ == FormattingTags.noFlowSection.names.head)

  private def isMarkdown(tag: TaggingFragment, currentValue: Boolean) =
    if (tag.isEmpty) currentValue
    else tag.names.exists(_ == FormattingTags.markdownSection.names.head) && !tag.names.exists(_ == FormattingTags.noMarkdownSection.names.head)
}

object DefaultFragmentsFormatting extends DefaultFragmentsFormatting