package org.specs2
package specification
package core

import scalaz.Show
import data.Tag

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

  def url = header.show

  def linkText =
    if (alias.nonEmpty) alias else header.show

  def show =
    (if (before.nonEmpty) before else "") +
    (if (alias.nonEmpty) alias else header.show) +
    (if (after.nonEmpty) after else "") +
    (if (alias.nonEmpty) s"[${header.show}}]" else "")

}

case object br extends Description {
  def show = "\n"
}

case object start extends Description {
  def show = ""
}

case object end extends Description {
  def show = ""
}

case class tab(n: Int = 1) extends Description {
  def show = ""
}

case class backtab(n: Int = 1) extends Description {
  def show = ""
}

case class Marker(tag: Tag, isSection: Boolean = false, appliesToNext: Boolean = true) extends Description {
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

  def tag(ts: String*)       = Marker(Tag(ts:_*), isSection = false)
  def taggedAs(ts: String*)  = Marker(Tag(ts:_*), isSection = false, appliesToNext = false)
  def section(ts: String*)   = Marker(Tag(ts:_*), isSection = true)
  def asSection(ts: String*) = Marker(Tag(ts:_*), isSection = true, appliesToNext = false)

  implicit def showInstance: Show[Description] = new Show[Description] {
    override def shows(d: Description): String =
      d match {
        case RawText(t) => t
        case _ => d.toString
      }
  }
}

