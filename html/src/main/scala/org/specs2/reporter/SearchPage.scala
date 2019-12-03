package org.specs2
package reporter

import fp.syntax._
import control._
import html._
import io._
import specification.core.{Env, SpecStructure}
import producer._

/**
 * Functions used to create an index and a search page for the generated html pages
 */
case class SearchPage(logger: Logger = ConsoleLogger()) {

  /** create an index for all the specifications */
  def createIndex(env: Env, specifications: List[SpecStructure], options: HtmlOptions): Operation[Unit] =
    for {
      htmlPages <- Operation.delayed(Indexing.createIndexedPages(env, specifications, options.outDir))
      _         <- Producer.emitSync(htmlPages).fold(Indexing.indexFold(options.indexFile))
      _         <- createSearchPage(env, options)
    } yield ()

  /** create a search page, based on the specs2.html template */
  def createSearchPage(env: Env, options: HtmlOptions): Operation[Unit] = {
    import env.{fileSystem => fs}
    for {
      template <- fs.readFile(options.template) ||| logger.warnAndFail("No template file found at "+options.template.path, HtmlPrinter.RunAborted)
      content  <- makeSearchHtml(template, options)
      _        <- fs.writeFile(searchFilePath(options), content)
    } yield ()
  }

  /** create the html search page content */
  def makeSearchHtml(template: String, options: HtmlOptions): Operation[String] = {
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
