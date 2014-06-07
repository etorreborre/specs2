package org.specs2
package specification
package core

import scalaz.Show
import org.specs2.data.{NamedTag, Tag}

trait Description {
  def show: String
  def matches(s: String) = false
}

case class RawText(text: String) extends Description {
  def show: String = text
  override def matches(s: String) = text matches s
}

case class Code(md: String) extends Description {
  def show: String = md
  override def matches(s: String) = md matches s
}

case object NoText extends Description {
  def show: String = ""
}

case class SpecificationLink(header: SpecHeader, before: String = "", after: String = "", alias: String = "", tooltip: String = "") extends Description {
  def specClassName = header.className

  def url = specClassName+".html"

  def linkText =
    if (alias.nonEmpty) alias else header.show

  def show =
    (if (before.nonEmpty) before else "") +
    (if (alias.nonEmpty) alias else header.show) +
    (if (after.nonEmpty) after else "") +
    (if (alias.nonEmpty) s"[${header.show}}]" else "")

}

case object Br extends Description {
  def show = "\n"
}

case object Start extends Description {
  def show = ""
}

case object End extends Description {
  def show = ""
}

case class Tab(n: Int = 1) extends Description {
  def show = ""
}

case class Backtab(n: Int = 1) extends Description {
  def show = ""
}

case class Marker(tag: NamedTag, isSection: Boolean = false, appliesToNext: Boolean = true) extends Description {
  def show = ""
}

object AlwaysMarker extends Description {
  def show = ""
}

object AlwaysWhenNoIncludeMarker extends Description {
  def show = ""
}

object Description {
  def text(text: String) = RawText(text)

  def tag(ts: String*)       = mark(Tag(ts:_*))
  def taggedAs(ts: String*)  = markAs(Tag(ts:_*))
  def section(ts: String*)   = markSection(Tag(ts:_*))
  def asSection(ts: String*) = markSectionAs(Tag(ts:_*))

  def mark(tag: NamedTag)          = Marker(tag, isSection = false)
  def markAs(tag: NamedTag)        = Marker(tag, isSection = false, appliesToNext = false)
  def markSection(tag: NamedTag)   = Marker(tag, isSection = true)
  def markSectionAs(tag: NamedTag) = Marker(tag, isSection = true, appliesToNext = false)

  implicit def showInstance: Show[Description] = new Show[Description] {
    override def shows(d: Description): String =
      d match {
        case RawText(t) => t
        case _ => d.toString
      }
  }
}

