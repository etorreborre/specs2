package org.specs2
package specification

import core._
import matcher._
import dsl.ReferenceDsl
import org.specs2.control.Debug

class HtmlLinksSpec extends Spec with TypedEqual with ReferenceDsl { def is = s2"""

  Html links referencing specifications can be introduced

  for a specification
  ${ a(link(userGuide))                             === "<a href='org.specs2.specification.UserGuideSpecification.html'>User guide</a>" }
  ${ a(link(howTo))                                 === "<a href='org.specs2.specification.HowTo.html'>How to</a>" }

  with a specific alias
  ${ a("User Guide" ~ userGuide)                    === "<a href='org.specs2.specification.UserGuideSpecification.html'>User Guide</a>" }
  ${ a("learn" ~ howTo)                             === "<a href='org.specs2.specification.HowTo.html'>learn</a>" }

  with a tooltip
  ${ a("user guide" ~ (userGuide, "this one"))      === "<a href='org.specs2.specification.UserGuideSpecification.html' tip='this one'>user guide</a>" }
  """

  def a(f: Fragment) = f match {
    case Fragment(link : SpecificationRef, _, _) =>
      s"""<a href='${link.url}'${if (link.tooltip.isEmpty) "" else s" tip='${link.tooltip}'"}>${link.linkText}</a>""".trim
    case other => "not a link"
  }

  lazy val userGuide = new UserGuideSpecification

  lazy val howTo = new HowTo
}
// a specification with no title
class HowTo extends Specification { def is = "" }
class UserGuideSpecification extends Specification {  def is = "User guide".title }
