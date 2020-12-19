package org.specs2
package reporter

import fp.syntax._
import io._
import FileName._
import main.Arguments
import specification.core.{OwnEnv, Env, SpecificationStructure}
import matcher._
import control._

class HtmlPrinterSpec(val env: Env) extends Specification with ActionMatchers with ThrownExpectations with OwnEnv { def is = sequential ^ s2"""

 The Html printer outputs html files for a specification and its linked specification

   if html.search == true then it creates an index contents file $index
   if html.search == true then it creates a search page          $searchPage

"""

  def index =
    val spec = new Specification { def is = s2""" one example $ok """}
    val env1 = env.setArguments(searchArguments)

    printer(env1).getHtmlOptions(env1.arguments).map(_.search).runOption must beSome(true)

    finalize(env1, spec) must beOk
    FilePathReader.exists(outDir / "javascript" / "tipuesearch" | "tipuesearch_contents.js").runOption must beSome(true)


  def searchPage =
    val spec = new Specification { def is = s2""" one example $ok """}
    val env1 = env.setArguments(searchArguments)

    finalize(env1, spec) must beOk
    FilePathReader.exists(outDir | "search.html").runOption must beSome(true)

  def finalize(env: Env, spec: SpecificationStructure): Action[Unit] =
    val htmlPrinter = printer(env)
    for
      options <- htmlPrinter.getHtmlOptions(env.arguments).toAction
      _       <- htmlPrinter.copyResources(env, options).toAction
      _       <- htmlPrinter.finalize(List(spec.structure))
    yield ()


  def printer(env: Env) = HtmlPrinter(env, SearchPage())

  val outDir = "target" / "test" / "HtmlPrinterSpec"
  val searchArguments = Arguments.split(s"html.search html.outdir ${outDir.path}")
}
