package org.specs2
package reporter

import io._
import main.Arguments
import specification.core.Env
import matcher._
import control._
import execute._
import scalaz._,Scalaz._

class HtmlPrinterSpec extends Specification with TaskMatchers with ThrownExpectations { def is = s2"""

 The Html printer outputs html files for a specification and its linked specification

   if html.index == true then it creates an index contents file $index

"""

  def index = { env: Env =>
    val spec = new Specification { def is = s2""" one example $ok """}
    val env1 = env.copy(arguments = Arguments("html.index"))

    printer.getHtmlOptions(env1.arguments).map(_.createIndex).toTask must returnValue(true)

    printer.finalize(env1, List(spec)).toTask must returnOk
    FilePathReader.exists("target" / "specs2-reports" / "javascript" / "tipuesearch" | "tipuesearch_contents.js")
  }

  val printer = HtmlPrinter
}
