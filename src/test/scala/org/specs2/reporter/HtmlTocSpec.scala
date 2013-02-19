package org.specs2
package reporter

import internal.scalaz.Scalaz._
import main.Arguments
import specification.{ExecutedText, HtmlLink, SpecName}


class HtmlTocSpec extends Specification { def is =

  "if the notoc argument is used from the command-line, then no toc must be included"                                   ! notoc1^
  "the root file specifies can specify 'notoc' but children files can have their own"                                   ! notoc2^
                                                                                                                        end


  def notoc1 = printer.addToc(args.report(notoc=true))(singleFile).head.toc.toc must beEmpty

  def notoc2 = printer.addToc(args())(treeWithNoTocAtRoot).map(_.toc.toc.isEmpty) must_== Seq(true, false)


  val singleFile          = htmlLines("single")
  val treeWithNoTocAtRoot = htmlLines("root", args.report(notoc=true)).loc.insertDownLast(
                            htmlLines("child", args.report(notoc=false))).toTree

  def htmlLines(name: String, arguments: Arguments = Arguments()) = new HtmlLinesFile(SpecName(name), arguments, HtmlLink(this)).add(headerLine).leaf
  def headerLine = HtmlText(ExecutedText("### title"))

  val printer = new HtmlPrinter {}
}