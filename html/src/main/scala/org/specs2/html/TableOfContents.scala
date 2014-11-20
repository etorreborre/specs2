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
import text.Trim._

/**
 * This trait checks for the presence of a <toc/> tag at the beginning of a xml document and replaces it
 * by a list of links to the headers of the document
 */
trait TableOfContents {

  /** create a table of contents for all the specifications */
  def createToc(specifications: List[SpecStructure], outDir: DirectoryPath, fileSystem: FileSystem): Action[Unit] = for {
    pages   <- readHtmlPages(specifications, outDir, fileSystem)
    toc     =  createToc(pages, outDir)
    _       <- saveHtmlPages(pages.map(page => page.addToc(toc(page))), fileSystem)
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
      if (paths contains path) Some(fileSystem.readFile(path).map(content => SpecHtmlPage(spec, path, outDir, content)))
      else None
    }.flatten.sequenceU
  }

  def createToc(pages: List[SpecHtmlPage], outDir: DirectoryPath): SpecHtmlPage => NodeSeq = { page: SpecHtmlPage =>
    val dependsOn = (p1: SpecHtmlPage, p2: SpecHtmlPage) => SpecStructure.dependsOn(p1.specification, p2.specification)

    def li(page: SpecHtmlPage) =
      <li id={page.pandocName}><a href={page.path.relativeTo(outDir).path} title={page.showWords}>{page.showWords.truncate(15)}</a>
        <ul>{page.createSubtoc}</ul>
      </li>

    pages match {
      case Nil => NodeSeq.Empty
      case main :: rest =>
        val (dependents, others) = rest.partition(p => dependsOn(main, p))
        val highLevelItems: NodeSeq =
        <ul>
          {li(main)}
          {dependents.sortBy(linkIndexIn(main.specification.specificationLinks)).map(li).reduceNodes}
          <li><a>Others</a>
            <ul>{others.map(li).reduceNodes}</ul>
          </li>
        </ul>

        val result: NodeSeq =
          <div id="tree">{highLevelItems}</div> ++
          <script>{s"$$(function () { $$('#tree').jstree({'core':{'initially_open':['${main.pandocName}','${page.pandocName}'], 'animation':200}, 'themes' : {'theme': 'default','url': './css/themes/default/style.css'}, 'plugins':['themes', 'html_data']}); });"}</script>

        result
    }
  }

  /** @return the index of a linked specification in 'main' */
  def linkIndexIn(s1Links: Seq[SpecificationLink]): SpecHtmlPage => Int = { s2: SpecHtmlPage =>
    s1Links.map(_.specClassName).indexOf(s2.className)
  }


  def saveHtmlPages(pages: List[SpecHtmlPage], fileSystem: FileSystem): Action[Unit] =
    pages.map(page => fileSystem.writeFile(page.path, page.content)).sequenceU.void

}

object TableOfContents extends TableOfContents
