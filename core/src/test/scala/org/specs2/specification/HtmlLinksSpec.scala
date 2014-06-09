package org.specs2
package specification

import core._

class HtmlLinksSpec extends Specification { def is = s2"""

  Html links referencing specifications can be introduced

  with some text before
  ${ a("a" ~ userGuide)                             === "a <a href='org.specs2.specification.UserGuideSpecification.html'>User guide</a>" }
  ${ a("learn" ~ howTo)                             === "learn <a href='org.specs2.specification.HowTo.html'>HowTo</a>" }

  with a way to change the highlighted text
  ${ a("a" ~ ("User Guide", userGuide))             === "a <a href='org.specs2.specification.UserGuideSpecification.html'>User Guide</a>" }
  ${ a("learn" ~ ("How to", howTo))                 === "learn <a href='org.specs2.specification.HowTo.html'>How to</a>" }

  with some text after
  ${ a("a" ~ (userGuide, "for you"))                === "a <a href='org.specs2.specification.UserGuideSpecification.html'>User guide</a> for you" }
  ${ a("learn" ~ (howTo, "do it"))                  === "learn <a href='org.specs2.specification.HowTo.html'>HowTo</a> do it" }
  ${ a("learn" ~ ("How to", howTo, "do it"))        === "learn <a href='org.specs2.specification.HowTo.html'>How to</a> do it" }

  with some text after but no text before
  ${ a("" ~ (userGuide, "for you"))                 === "<a href='org.specs2.specification.UserGuideSpecification.html'>User guide</a> for you" }
  ${ a("" ~ (howTo, "do it"))                       === "<a href='org.specs2.specification.HowTo.html'>HowTo</a> do it" }
  ${ a("" ~ ("How to", howTo, "do it"))             === "<a href='org.specs2.specification.HowTo.html'>How to</a> do it" }

  with a tooltip
  ${ a("a" ~ (userGuide, "for you", "yes"))         === "a <a href='org.specs2.specification.UserGuideSpecification.html' tip='yes'>User guide</a> for you" }
  ${ a("learn" ~ (howTo, "do it", "yes"))           === "learn <a href='org.specs2.specification.HowTo.html' tip='yes'>HowTo</a> do it" }
  ${ a("learn" ~ ("How to", howTo, "do it", "yes")) === "learn <a href='org.specs2.specification.HowTo.html' tip='yes'>How to</a> do it" }
  """

  def a(f: Fragment) = f match {
    case Fragment(link @ SpecificationLink(_,_,_,_,_), _, _) =>
      s"""${link.before} <a href='${link.url}'${if (link.tooltip.isEmpty) "" else s" tip='${link.tooltip}'"}>${link.linkText}</a> ${link.after}""".trim
    case other => "not a link"
  }

  lazy val userGuide = new UserGuideSpecification

  lazy val howTo = new HowTo
}
// a specification with no title
class HowTo extends Specification { def is = "" }
class UserGuideSpecification extends Specification {  def is = "User guide".title }
