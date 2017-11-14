package org.specs2
package reporter

import io.DirectoryPath
import main.Arguments
import org.specs2.matcher.XmlMatchers
import org.specs2.specification.Forms
import specification.process.Level
import specification.core.Fragment

import scala.xml.NodeSeq
import org.specs2.text.AnsiColors

object HtmlBodyPrinterSpec extends Specification with Forms with XmlMatchers { def is = s2"""

 A hidden reference must not be printed $hidden
 A form must be printed $printForm

 Ansi colors must be removed in text fragments $ansiColors
"""

  def hidden = {
    print(link(HtmlBodyPrinterSpec).hide) must beEmpty
  }

  def printForm = {
    val ns: NodeSeq = print(formFragmentFactory.FormFragment(form("hey").tr(prop("test", 1, 2))))
    ns must \\(<form></form>)
  }

  def ansiColors = {
    print(fragmentFactory.text(AnsiColors.color("text", AnsiColors.red))).toString must_== "text"
  }

  def print(f: Fragment): NodeSeq =
    HtmlBodyPrinter.printFragment(f, success, Arguments(), Level.Root, DirectoryPath.Root, pandoc = true)
}
