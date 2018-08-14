package org.specs2
package reporter

import io.DirectoryPath
import main.Arguments
import org.specs2.concurrent.ExecutionEnv
import org.specs2.control.ExecuteActions._
import org.specs2.matcher.XmlMatchers
import org.specs2.specification.Forms
import specification.process.{Level, Stats}
import specification.core.{Fragment, SpecStructure}

import scala.xml.NodeSeq
import org.specs2.text.AnsiColors
import org.specs2.time.SimpleTimer

class HtmlBodyPrinterSpec(ee: ExecutionEnv) extends Specification with Forms with XmlMatchers { def is = s2"""

 A hidden reference must not be printed $hidden
 A form must be printed $printForm

 Ansi colors must be removed in text fragments $ansiColors

 Fragments must have proper levels in html body $htmlBody
"""

  def hidden = {
    print(link(new spec { def is = s2"""
        Explanation Text.
       example no. one $ok"""}).hide) must beEmpty
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

  trait spec extends org.specs2.Specification

  def htmlBody = {
    val fragments = new spec { def is = s2"""
        Explanation Text.

        First starting test line

          example no. one $ok
          example no. two $ok

        Second starting test line
          first set of fragments $fragments1
          second set of fragments $fragments2
          """
    }.is.fragments
    println(fragments.contents.runList.runOption(ee))

    makeBody(fragments)(ee).map(_.trim.replaceAll("\\s+","")).runOption === Some(
      """
        |<text class="ok">
        |   <br/>Explanation Text. First starting test line<br/>
        |</text>
        |<ul>
        |   <li class="example skipped ok">
        |       <text>example no. one</text>
        |       <br/>
        |       <message class="skipped"></message>
        |   </li>
        |</ul>
        |<ul>
        |   <li class="example skipped ok">
        |       <text>example no. two</text>
        |       <br/>
        |       <message class="skipped"></message>
        |   </li>
        |</ul>
        |<text class="ok">
        |   <br/>Second starting test line first set of fragments
        |</text>
        |<br/>
        |<br/>
        |<li class="example skipped ok">
        |   <text>example no. one from first set</text>
        |   <br/>
        |   <message class="skipped"></message>
        |</li>
        |<br/>
        |<li class="example skipped ok">
        |   <text>example no. two from first set</text>
        |   <br/>
        |   <message class="skipped"></message>
        |</li>
        |<br/>
        |<text class="ok">
        |   <br/>second set of fragments
        |</text>
        |<br/>
        |<br/>
        |<li class="example skipped ok">
        |   <text>example no. one from second set</text>
        |   <br/>
        |   <message class="skipped"></message>
        |</li>
        |<br/>
        |<li class="example skipped ok">
        |   <text>example no. two from second set</text>
        |   <br/>
        |   <message class="skipped"></message>
        |</li>
        |<br/>
      """.stripMargin.trim.replaceAll("\\s+",""))
  }

  private def fragments1 = fragments("first")
  private def fragments2 = fragments("second")

  private def fragments(description: String) = {
    p^
      s"example no. one from $description set"  ! success ^br^
      s"example no. two from $description set"  ! success ^br
  }

  private def makeBody(specStructure: SpecStructure)(executionEnv: ExecutionEnv) =
    HtmlBodyPrinter.makeBody(specStructure, Stats(), new SimpleTimer(), options, Arguments(), pandoc = false)(executionEnv)

  def options = HtmlOptions(
    outDir = HtmlOptions.outDir,
    baseDir = HtmlOptions.baseDir,
    template = HtmlOptions.outDir.toFilePath,
    variables = Map(),
    noStats = true,
    search = false,
    toc = false,
    tocEntryMaxSize = 18,
    warnMissingSeeRefs = true)
}
