package org.specs2
package html

import scala.xml.*
import specification.core.*
import control.*
import io.*
import xml.Nodex.{given, *}
import Htmlx.*
import fp.*, syntax.*
import data.Trees.*
import concurrent.ExecutionEnv
import text.Trim.*

/** This trait checks for the presence of a <toc/> tag at the beginning of a xml document and replaces it by a list of
  * links to the headers of the document
  */
trait TableOfContents:

  /** create a table of contents for all the specifications */
  def createToc(
      env: Env,
      specifications: List[SpecStructure],
      outDir: DirectoryPath,
      entryMaxSize: Int,
      fileSystem: FileSystem
  ): Operation[Unit] =
    // sort specifications a, b, c so that a depends on b and c
    val sorted =
      SpecStructure.reverseTopologicalSort(specifications)(env.specs2ExecutionEnv).map(_.toList).getOrElse(List())
    for
      pages <- readHtmlPages(sorted, outDir, fileSystem)
      toc = createToc(pages, outDir, entryMaxSize)(using env.specs2ExecutionEnv)
      _ <- saveHtmlPages(pages.map(page => page.addToc(toc(page))), fileSystem)
    yield ()

  /** read the generated html pages and return them as a tree, based on the links relationships between them */
  def readHtmlPages(
      specifications: List[SpecStructure],
      outDir: DirectoryPath,
      fileSystem: FileSystem
  ): Operation[List[SpecHtmlPage]] =
    for
      paths <- fileSystem.listFilePaths(outDir)
      pages <- createSpecPages(paths, specifications, outDir, fileSystem)
    yield pages

  def createSpecPages(
      paths: List[FilePath],
      specifications: List[SpecStructure],
      outDir: DirectoryPath,
      fileSystem: FileSystem
  ): Operation[List[SpecHtmlPage]] =
    specifications.flatMap { spec =>
      val path = SpecHtmlPage.outputPath(outDir, spec)
      if paths contains path then
        Some(fileSystem.readFile(path).map(content => SpecHtmlPage(spec, path, outDir, content)))
      else None
    }.sequence

  def createToc(pages: List[SpecHtmlPage], outDir: DirectoryPath, entryMaxSize: Int)(using
      ee: ExecutionEnv
  ): SpecHtmlPage => NodeSeq =
    pages match
      case List() => (page: SpecHtmlPage) => NodeSeq.Empty
      case main :: rest =>
        val treeLoc = pagesTree(main, pages)
        val tocNodes: NodeSeq =
          treeLoc.cojoin.toTree.bottomUp { (pageTreeLoc: TreeLoc[SpecHtmlPage], subtocs: LazyList[NodeSeq]) =>
            val page = pageTreeLoc.getLabel
            <ul>
                {
              li(outDir, entryMaxSize)(page)(<ul>
                    {subtocs.reduceNodes}
                  </ul>)
            }
              </ul>: NodeSeq
          }.rootLabel

        (page: SpecHtmlPage) => {
          def isPageNode = (loc: TreeLoc[SpecHtmlPage]) => loc.getLabel == page
          val parentNames = treeLoc
            .find(isPageNode)
            .map(n => (n.parents.map(_._2.pandocName) :+ page.pandocName).map(name => "'" + name + "'").mkString(","))
            .getOrElse("'" + page.pandocName + "'")
          val result =
            <div id="tree">{tocNodes}</div> ++
              <script>{
                s"$$(function () { $$('#tree').jstree({'core':{'initially_open':[$parentNames], 'animation':200}, 'themes' : {'theme': 'default','url': './css/themes/default/style.css'}, 'plugins':['themes', 'html_data']}); });"
              }</script>
          result
        }

  def pagesTree(page: SpecHtmlPage, pages: List[SpecHtmlPage])(using ee: ExecutionEnv): TreeLoc[SpecHtmlPage] =
    Tree
      .unfoldTree((page, (pages, List[SpecHtmlPage]()))) { current =>
        val (p1: SpecHtmlPage, (remaining, visited)) = current
        val (dependents, others) =
          remaining.partition(p2 => p1.specification.dependsOn(p2.specification)(ee) && !visited.contains(p2))
        val distinctDependents = dependents.groupBy(_.className).view.mapValues(_.head).values.toList
        val visited1 = distinctDependents ::: visited
        (
          (p1, (others, visited1)),
          () =>
            distinctDependents
              .sortBy(linkIndexIn(p1.specification.linkReferencesList))
              .to(LazyList)
              .map((_, (others, visited1)))
        )
      }
      .loc
      .map(_._1)

  /** @return the index of a linked specification in 'main' */
  def linkIndexIn(s1Refs: Seq[SpecificationRef]): SpecHtmlPage => Int = { (s2: SpecHtmlPage) =>
    s1Refs.map(_.specClassName).indexOf(s2.className)
  }

  def li(outDir: DirectoryPath, entryMaxSize: Int)(page: SpecHtmlPage)(nested: NodeSeq): NodeSeq =
    <li id={page.pandocName}><a href={page.path.relativeTo(outDir).path} title={page.showWords}>{
      page.showWords.truncate(entryMaxSize)
    }</a>
      <ul>{createHeadersSubtoc(page, entryMaxSize)}</ul>
      {nested}
    </li>

  def createHeadersSubtoc(page: SpecHtmlPage, entryMaxSize: Int): NodeSeq =
    page.body.headersTree.bottomUp { (h: Header, s: LazyList[NodeSeq]) =>
      if h.isRoot then
        // 'id' is the name of the attribute expected by jstree to "open" the tree on a specific node
        s.reduceNodes.updateHeadAttribute("id", page.path.name.name)
      else if h.level > 1 then
        <li><a href={page.relativePath.path + "#" + h.pandocName} title={h.name}>{h.name.truncate(entryMaxSize)}</a>
            {<ul>{s}</ul> `orEmptyWhen` s.isEmpty}
          </li>
      else <ul>{s}</ul> `orEmptyWhen` s.isEmpty
    }.rootLabel

  def saveHtmlPages(pages: List[SpecHtmlPage], fileSystem: FileSystem): Operation[Unit] =
    pages.map(page => fileSystem.writeFile(page.path, page.content)).sequence.void

object TableOfContents extends TableOfContents
