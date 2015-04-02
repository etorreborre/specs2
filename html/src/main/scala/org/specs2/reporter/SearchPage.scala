package org.specs2
package reporter

import control._
import data.Fold
import html.{HtmlTemplate, Indexing}
import io._
import org.specs2.specification.core.{SpecStructure, Env, SpecificationStructure}

import scalaz.stream.Process

/**
 * Functions used to create an index and a search page for the generated html pages
 */
trait SearchPage {

  /** create an index for all the specifications */
  def createIndex(env: Env, specifications: List[SpecStructure], options: HtmlOptions): Action[Unit] =
    for {
      htmlPages <- Actions.safe(Indexing.createIndexedPages(env, specifications, options.outDir))
      _         <- Fold.runFold(Process.emitAll(htmlPages), Indexing.indexFold(options.indexFile)).toAction
      _         <- createSearchPage(env, options)
    } yield ()

  /** create a search page, based on the specs2.html template */
  def createSearchPage(env: Env, options: HtmlOptions): Action[Unit] = {
    import env.fileSystem._
    for {
      template <- readFile(options.template) ||| warnAndFail("No template file found at "+options.template.path, HtmlPrinter.RunAborted)
      content  <- makeSearchHtml(template, options)
      _        <- writeFile(searchFilePath(options), content)
    } yield ()
  }

  /** create the html search page content */
  def makeSearchHtml(template: String, options: HtmlOptions): Action[String] = {
    val variables1 =
      options.templateVariables
        .updated("title", "Search")
        .updated("path", searchFilePath(options).path)

    HtmlTemplate.runTemplate(template, variables1)
  }

  /** search page path */
  def searchFilePath(options: HtmlOptions): FilePath =
    options.outDir | "search.html"

}

object SearchPage extends SearchPage
