package org.specs2
package reporter

import mutable._
import ExecutedSpecificationData._
import io.MockWriter
import specification.SpecificationStructure

class MarkupExportingSpec extends Specification {

  "A specification with markup text can be exported with no interpretation of the markup text" >> {
    export(introduction) must contain("this is a simple spec with some *markdown* text")
  }

  "The SpecStart is translated as a ## header" >> {
    export(introduction) must contain("## Intro")
  }

  "A link is translated as a bullet point" >> {
    export(introduction) must contain("* <a tooltip=\"\" href=\"other.html\">a link to the rest</a>")
  }

  def export(spec: SpecificationStructure) = {
    val e = exporter
    e.export(args())(spec)
    e.out.messages.mkString("\n")
  }

  val introduction = new Specification {
    "Intro".title
    "this is a simple spec with some *markdown* text".p
    "a link to the rest" ~ other
  }

  def other = new Specification { "other".title }

  def exporter = new Exporter

  class Exporter extends MarkupExporting {
    val out = new MockWriter {}
    override protected def writeFile = (file: HtmlFile) => writeXml(file.xml)(out)
  }
}
