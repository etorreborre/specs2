package org.specs2
package reporter

import internal.scalaz.Scalaz._
import specification.{HtmlLink, SpecName}


class HtmlTocSpec extends Specification { def is =

  "if the notoc argument is used then no toc must be included" ! notoc1


  def notoc1 = printer.createToc(file)(args.report(notoc=true)).toc must be empty

  val file    = new HtmlLinesFile(SpecName("name"), HtmlLink(this), Seq[HtmlLine]()).leaf
  val printer = new HtmlPrinter {}
}