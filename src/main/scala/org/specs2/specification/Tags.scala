package org.specs2
package specification

import TagsFragments._

/**
 * The tags trait allows the creation of Tags fragments in a Specification
 */
trait Tags {
  /** create a TaggedAs fragment */
  def tag(names: String*): TaggingFragment = TaggedAs(names:_*)
  /** create a AsSection fragment */
  def section(names: String*): TaggingFragment = AsSection(names:_*)
}
object Tags extends Tags

trait FormattingTags extends Tags {
  def markdownSection   = section("specs2.internal.markdown")
  def noMarkdown        = noMarkdownSection
  def noMarkdownSection = section("specs2.internal.nomarkdown")
  def flowSection       = section("specs2.internal.flow")
  def noFlowSection     = section("specs2.internal.noflow")
  def noFlow            = noFlowSection
}

object FormattingTags extends FormattingTags