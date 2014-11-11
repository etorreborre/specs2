package org.specs2
package html

import control._
import specification.core._
import scala.xml.NodeSeq
import io._
import xml.Nodex._
import Htmlx._
import data.Trees._
import scalaz._, Scalaz._

/**
 * This trait checks for the presence of a <toc/> tag at the beginning of a xml document and replaces it
 * by a list of links to the headers of the document
 */
trait TableOfContents {

  /** create a table of contents for all the specifications */
  def createToc(specifications: List[SpecStructure], outDir: DirectoryPath, fileSystem: FileSystem): Action[Unit] = for {
    pages   <- readHtmlPages(specifications, outDir, fileSystem)
    toc     =  createToc(pages)
    _       <- saveHtmlPages(pages.map(_.addToc(toc)), fileSystem)
  } yield ()

  /** read the generated html pages and return them as a tree, based on the links relationships between them */
  def readHtmlPages(specifications: List[SpecStructure], outDir: DirectoryPath, fileSystem: FileSystem): Action[List[SpecHtmlPage]] =
    for {
      paths <- fileSystem.listFilePaths(outDir)
      pages <- createSpecPages(paths.toList, specifications, outDir, fileSystem)
    } yield pages

  def createSpecPages(paths: List[FilePath], specifications: List[SpecStructure], outDir: DirectoryPath, fileSystem: FileSystem): Action[List[SpecHtmlPage]] = {
    specifications.map { spec =>
      val path = SpecHtmlPage.outputPath(outDir, spec)
      if (paths contains path) Some(fileSystem.readFile(path).map(content => SpecHtmlPage(spec, path, content)))
      else None
    }.flatten.sequenceU
  }

  def createToc(pages: List[SpecHtmlPage]): NodeSeq = {
    val dependsOn = (p1: SpecHtmlPage, p2: SpecHtmlPage) => SpecStructure.dependsOn(p1.specification, p2.specification)
    def li(page: SpecHtmlPage) =
      <li id={page.className}><a href={page.path.path}>{page.showWords}</a>
        <ul>{page.createSubtoc}</ul>
      </li>

    pages match {
      case Nil => NodeSeq.Empty
      case main :: rest =>
        val (dependents, others) = rest.partition(p => dependsOn(main, p))
        val highLevelItems: NodeSeq =
        <ul>
          {li(main)}
          {dependents.map(li).reduceNodes}
          <li><a>Others</a>
            <ul>{others.map(li).reduceNodes}</ul>
          </li>
        </ul>

        val result: NodeSeq =
          <div id="tree">{highLevelItems}</div> ++
          <script>{"$(function () { $('#tree').jstree({'core':{'initially_open':['206794297','740479'], 'animation':200}, 'plugins':['themes', 'html_data']}); });"}</script>

        result
    }
  }

  def saveHtmlPages(pages: List[SpecHtmlPage], fileSystem: FileSystem): Action[Unit] =
    pages.map(page => fileSystem.writeFile(page.path, page.content)).sequenceU.void

}

object TableOfContents extends TableOfContents
