package org.specs2
package reporter

import data.Fold
import io.{FilePath, FileSystem}
import specification.core.{Env, SpecificationStructure}

import scalaz.concurrent.Task
import scalaz.stream._

/**
 * Fold functions to create index files
 */
object Indexing {

  val indexState = { (page: IndexedPage, index: Index) =>
    index.add(createEntries(page))
  }

  def indexFold(path: FilePath) = new Fold[IndexedPage] {
    type S = Index

    def prepare: Task[Unit] = Task.now(())
    def sink: Sink[Task, (IndexedPage, Index)] = Fold.unitSink

    def fold = indexState
    def init = Index.empty
    def last(index: Index): Task[Unit] = FileSystem.writeFileTask(path, Index.toJson(index))
  }

  def createIndexedPages(env: Env, specifications: List[SpecificationStructure], options: HtmlOptions): Seq[IndexedPage] = {
    specifications.map(createIndexedPage(env, options))
  }

  def createIndexedPage(env: Env, options: HtmlOptions) = (specification: SpecificationStructure) => {
    val spec = specification.structure(env)
    IndexedPage(
      path     = HtmlPrinter.outputFilePath(options.outDir, spec),
      title    = spec.header.wordsTitle,
      contents = spec.texts.foldLeft(new StringBuilder)((res, cur) => res.append(cur.description.show)).toString)
  }

  def createEntries(page: IndexedPage): Vector[IndexEntry] = {
    Vector(IndexEntry(page.title, page.contents, Vector(), page.path))
  }
}

case class IndexedPage(path: FilePath, title: String, contents: String)

case class Index(entries: Vector[IndexEntry]) {
  def add(entry: IndexEntry) = copy(entries :+ entry)
  def add(other: Seq[IndexEntry]) = copy(entries ++ other)
}

object Index {

  val empty = Index(Vector())

  def toJson(index: Index): String = {
    s"""
       |var tipuesearch = {"pages": ${pages(index).mkString("[", ",\n", "]")}}
     """.stripMargin
  }

  def pages(index: Index): Seq[String] =
    index.entries.map(page)

  def page(entry: IndexEntry): String =
    s"""{"title":"${entry.title}", "text":"${entry.text}", "tags":${entry.tags.map("\""+_+"\"").mkString("[",",", "]")}, "loc":"${entry.path}"}"""

}

case class IndexEntry(title: String, text: String, tags: Vector[String], path: FilePath)

