package org.specs2
package reporter

import io._
import main.Arguments
import org.specs2.specification.BeforeAll
import org.specs2.specification.core.{SpecificationStructure, Env}
import matcher._
import control._
import execute._
import scalaz._,Scalaz._

class HtmlPrinterSpec extends Specification with TaskMatchers with ThrownExpectations { def is = sequential ^ s2"""

 The Html printer outputs html files for a specification and its linked specification

   if html.search == true then it creates an index contents file $index
   if html.search == true then it creates a search page          $searchPage

"""

  def index = { env: Env =>
    val spec = new Specification { def is = s2""" one example $ok """}
    val env1 = env.copy(arguments = searchArguments)

    printer.getHtmlOptions(env1.arguments).map(_.search).toTask must returnValue(true)

    finalize(env1, spec).toTask must returnOk
    FilePathReader.exists(outDir / "javascript" / "tipuesearch" | "tipuesearch_contents.js")
  }


  def searchPage = { env: Env =>
    val spec = new Specification { def is = s2""" one example $ok """}
    val env1 = env.copy(arguments = searchArguments)

    finalize(env1, spec).toTask must returnOk
    FilePathReader.exists(outDir | "search.html")
  }

  def finalize(env: Env, spec: SpecificationStructure): Action[Unit] =
    for {
      options <- printer.getHtmlOptions(env.arguments)
      _       <- printer.copyResources(env, options)
      _       <- printer.finalize(env, List(spec.structure(env)))
    } yield ()


  val printer = HtmlPrinter

  val outDir = "target" / "test" / "HtmlPrinterSpec"
  val searchArguments = Arguments.split(s"html.search html.outdir ${outDir.path}")
}
