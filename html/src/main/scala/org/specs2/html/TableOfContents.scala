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
    pages match {
      case Nil => NodeSeq.Empty
      case main :: rest =>
        val treeLoc = pagesTree(main, pages)
        val tocNodes =
          treeLoc.cojoin.toTree.bottomUp { (pageTreeLoc: TreeLoc[SpecHtmlPage], subtocs: Stream[NodeSeq]) =>
            val page = pageTreeLoc.getLabel
            <ul>
              {li(outDir)(page)}
              {subtocs.reduceNodes}
              <ul>{createHeadersSubtoc(page)}</ul>
            </ul>
          }

        def isPageNode = (loc: TreeLoc[SpecHtmlPage]) => loc.getLabel == page

        val parentNames = treeLoc.find(isPageNode).map(_.parents.map(_._2.pandocName).map(name => "'"+name+"'").mkString(",")).getOrElse("")
        val result =
        <div id="tree">{tocNodes}</div> ++
          <script>{s"$$(function () { $$('#tree').jstree({'core':{'initially_open':[$parentNames], 'animation':200}, 'themes' : {'theme': 'default','url': './css/themes/default/style.css'}, 'plugins':['themes', 'html_data']}); });"}</script>

        result
    }
  }

  def pagesTree(page: SpecHtmlPage, pages: List[SpecHtmlPage]): TreeLoc[SpecHtmlPage] =
    Tree.unfoldTree((page, List[SpecHtmlPage]())) { current =>
      val (p1: SpecHtmlPage, visited: List[SpecHtmlPage]) = current
      val visited1 = p1 :: visited
      ((p1, visited1), () => pages.filter(p2 => SpecStructure.dependsOn(p1.specification, p2.specification) && !visited1.contains(p2)).toStream.map((_, visited1)))
    }.loc.map(_._1)

  /** @return the index of a linked specification in 'main' */
  def linkIndexIn(s1Links: Seq[SpecificationLink]): SpecHtmlPage => Int = { s2: SpecHtmlPage =>
    s1Links.map(_.specClassName).indexOf(s2.className)
  }

  def li(outDir: DirectoryPath)(page: SpecHtmlPage) =
    <li id={page.pandocName}><a href={page.path.relativeTo(outDir).path} title={page.showWords}>{page.showWords.truncate(15)}</a>
      <ul>{createHeadersSubtoc(page)}</ul>
    </li>


  def createHeadersSubtoc(page: SpecHtmlPage): NodeSeq = {
    page.body.headersTree.
      bottomUp { (h: Header, s: Stream[NodeSeq]) =>
      if (h.isRoot)
        // 'id' is the name of the attribute expected by jstree to "open" the tree on a specific node
        s.reduceNodes.updateHeadAttribute("id", page.path.name.name)
      else if (h.level > 1)
        <li><a href={page.relativePath.path+"#"+h.pandocName} title={h.name}>{h.name.truncate(15)}</a>
          { <ul>{s.toSeq}</ul> unless s.isEmpty }
        </li>
      else
        <ul>{s.toSeq}</ul> unless s.isEmpty

    }.rootLabel
  }

  def saveHtmlPages(pages: List[SpecHtmlPage], fileSystem: FileSystem): Action[Unit] =
    pages.map(page => fileSystem.writeFile(page.path, page.content)).sequenceU.void

}

object TableOfContents extends TableOfContents
