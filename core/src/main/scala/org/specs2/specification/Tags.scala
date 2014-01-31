package org.specs2
package specification

import TagFragments._
import FormattingTags._

/**
 * The tags trait allows the creation of Tags fragments in a Specification
 */
trait Tags {
  /** create a TaggedAs fragment */
  def tag(names: String*): TagFragment = TaggedAs(names:_*)
  /** create a AsSection fragment */
  def section(names: String*): TagFragment = AsSection(names:_*)

  /** shortcut to add tag more quickly when rerunning failed tests */
  private[specs2] def xtag = tag("x")
  /** shortcut to add section more quickly when rerunning failed tests */
  private[specs2] def xsection = section("x")
}
object Tags extends Tags

/**
 *
 */
trait FormattingTags extends Tags {
  implicit def ToFormattingTagParameter(condition: Boolean): Option[FormattingTagParameter] = Some(FormattingTagParameter(condition))
  case class FormattingTagParameter(condition: Boolean)

  /**
   * add a tagging section with special tag names specifying the formatting options
   */
  def formatSection(markdown: Option[FormattingTagParameter] = None, flow: Option[FormattingTagParameter] = None, verbatim: Option[FormattingTagParameter] = None) =
    section(markdown.map(p => doIt(p)+"markdown").toSeq ++
            flow    .map(p => doIt(p)+"flow").toSeq ++
            verbatim.map(p => doIt(p)+"verbatim").toSeq:_*)

  /**
   * add a tag with special tag names specifying the formatting options
   */
  def formatTag(markdown: Option[FormattingTagParameter] = None, flow: Option[FormattingTagParameter] = None, verbatim: Option[FormattingTagParameter] = None) =
    tag(markdown.map(p => doIt(p)+"markdown").toSeq ++
        flow    .map(p => doIt(p)+"flow").toSeq ++
        verbatim.map(p => doIt(p)+"verbatim").toSeq:_*)

  private def doIt(p: FormattingTagParameter) = (if (p.condition) "" else "!")+internal

}

object FormattingTags extends FormattingTags {
  private[specs2] val internal = "specs2.internal."
}