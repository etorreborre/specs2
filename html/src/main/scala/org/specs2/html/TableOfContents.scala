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
    val expectedFiles = specifications.map(s => SpecHtmlPage.outputPath(outDir, s))
    paths.filter(expectedFiles.contains).map { path =>
      fileSystem.readFile(path).map(content => SpecHtmlPage(path, content))
    }.sequenceU
  }

  def createToc(pages: List[SpecHtmlPage]): NodeSeq =
    <div id="tree">{pages.map(_.createSubtoc).reduceNodes}</div> ++
      <script>{"$(function () { $('#tree').jstree({'core':{'initially_open':['206794297','740479'], 'animation':200}, 'plugins':['themes', 'html_data']}); });"}</script>

  def saveHtmlPages(pages: List[SpecHtmlPage], fileSystem: FileSystem): Action[Unit] =
    pages.map(page => fileSystem.writeFile(page.path, page.content)).sequenceU.void

}

object TableOfContents extends TableOfContents
