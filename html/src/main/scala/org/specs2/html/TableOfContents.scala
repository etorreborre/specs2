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
  def createToc(specifications: List[SpecStructure], outDir: DirectoryPath, entryMaxSize: Int, fileSystem: FileSystem): Action[Unit] = for {
    pages   <- readHtmlPages(specifications, outDir, fileSystem)
    toc     =  createToc(pages, outDir, entryMaxSize)
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

  def createToc(pages: List[SpecHtmlPage], outDir: DirectoryPath, entryMaxSize: Int): SpecHtmlPage => NodeSeq = {
    pages match {
      case Nil => (page: SpecHtmlPage) => NodeSeq.Empty
      case main :: rest =>
        val treeLoc = pagesTree(main, pages)
        val tocNodes: NodeSeq =
          treeLoc.cojoin.toTree.bottomUp { (pageTreeLoc: TreeLoc[SpecHtmlPage], subtocs: Stream[NodeSeq]) =>
            val page = pageTreeLoc.getLabel
              <ul>
                {li(outDir, entryMaxSize)(page)(
                  <ul>
                    {subtocs.reduceNodes}
                  </ul>)}
              </ul>: NodeSeq
          }.rootLabel

        (page: SpecHtmlPage) => {
          def isPageNode = (loc: TreeLoc[SpecHtmlPage]) => loc.getLabel == page
          val parentNames = treeLoc.find(isPageNode).map(n => (n.parents.map(_._2.pandocName) :+ page.pandocName).map(name => "'"+name+"'").mkString(",")).getOrElse(page.pandocName)
          val result =
            <div id="tree">{tocNodes}</div> ++
              <script>{s"$$(function () { $$('#tree').jstree({'core':{'initially_open':[$parentNames], 'animation':200}, 'themes' : {'theme': 'default','url': './css/themes/default/style.css'}, 'plugins':['themes', 'html_data']}); });"}</script>
          result
        }
    }
  }

  def pagesTree(page: SpecHtmlPage, pages: List[SpecHtmlPage]): TreeLoc[SpecHtmlPage] =
    Tree.unfoldTree((page, (pages, List[SpecHtmlPage]()))) { current =>
      val (p1: SpecHtmlPage, (remaining, visited)) = current
      val (dependents, others) = remaining.partition(p2 => p1.specification.dependsOn(p2.specification) && !visited.contains(p2))
      val visited1 = dependents ::: visited
      ((p1, (others, visited1)), () => dependents.sortBy(linkIndexIn(p1.specification.linkReferences)).toStream.map((_, (others, visited1))))
    }.loc.map(_._1)

  /** @return the index of a linked specification in 'main' */
  def linkIndexIn(s1Refs: Seq[SpecificationRef]): SpecHtmlPage => Int = { s2: SpecHtmlPage =>
    s1Refs.map(_.specClassName).indexOf(s2.className)
  }

  def li(outDir: DirectoryPath, entryMaxSize: Int)(page: SpecHtmlPage)(nested: NodeSeq): NodeSeq =
    <li id={page.pandocName}><a href={page.path.relativeTo(outDir).path} title={page.showWords}>{page.showWords.truncate(entryMaxSize)}</a>
      <ul>{createHeadersSubtoc(page, entryMaxSize)}</ul>
      {nested}
    </li>


  def createHeadersSubtoc(page: SpecHtmlPage, entryMaxSize: Int): NodeSeq = {
    page.body.headersTree.
      bottomUp { (h: Header, s: Stream[NodeSeq]) =>
      if (h.isRoot)
        // 'id' is the name of the attribute expected by jstree to "open" the tree on a specific node
        s.reduceNodes.updateHeadAttribute("id", page.path.name.name)
      else if (h.level > 1)
        <li><a href={page.relativePath.path+"#"+h.pandocName} title={h.name}>{h.name.truncate(entryMaxSize)}</a>
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
