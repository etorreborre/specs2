package org.specs2
package reporter

import specification._
import main.Arguments
import specification.TagsFragments.TaggingFragment

trait FragmentsFormatting {
  /** function returning a specification with a formatting applied to each fragment based on formatting tags */
  def format(implicit arguments: Arguments): SpecificationStructure => SpecificationStructure
}

trait DefaultFragmentsFormatting extends TagSelection {
  def format(implicit arguments: Arguments): SpecificationStructure => SpecificationStructure = (s: SpecificationStructure) =>
    SpecificationStructure(format(s.content.fragments))

  def format: Seq[Fragment] => Seq[Fragment] = (fs: Seq[Fragment]) =>
    fs.zip(tags(fs)).map {
      case (Text(formatted), tag)            => Text(formattedStringFor(tag)(formatted))
      case (e @ Example(formatted, _), tag)  => e.copy(desc = formattedStringFor(tag)(formatted))
      case (other, _)                        => other
    }

  private def formattedStringFor(tag: TaggingFragment) = (formatted: FormattedString) =>
    formatted.copy(formatting = formatted.formatting.copy(flow = isFlow(tag), markdown = isMarkdown(tag)))

  private def isFlow(tag: TaggingFragment) =
    tag.names.exists(_ == FormattingTags.flowSection.names.head) && !tag.names.exists(_ == FormattingTags.noFlowSection.names.head)

  private def isMarkdown(tag: TaggingFragment) =
    tag.names.exists(_ == FormattingTags.markdownSection.names.head) && !tag.names.exists(_ == FormattingTags.noMarkdownSection.names.head)
}

object DefaultFragmentsFormatting extends DefaultFragmentsFormatting