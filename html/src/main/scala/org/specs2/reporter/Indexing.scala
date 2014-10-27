package org.specs2
package reporter

import data.Fold
import io.{FilePath, FileSystem}
import specification.core.{Env, SpecificationStructure}

import scalaz.{Reducer, Monoid}
import scalaz.concurrent.Task
import scalaz.stream._

/**
 * Fold functions to create index files
 */
object Indexing {

  /**
   * An Index fold creates an Index page based on all pages to index and
   * saves it to a given file path
   */
  def indexFold(path: FilePath) =
    Fold.fromReducerAndLast(Index.reducer, (index: Index) => FileSystem.writeFileTask(path, Index.toJson(index)))

  def createIndexedPages(env: Env, specifications: List[SpecificationStructure], options: HtmlOptions): Seq[IndexedPage] = {
    specifications.map(createIndexedPage(env, options))
  }

  def createIndexedPage(env: Env, options: HtmlOptions) = (specification: SpecificationStructure) => {
    val spec = specification.structure(env)
    IndexedPage(
      path     = HtmlPrinter.outputFilePath(options, spec),
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
       |var tipuesearch = {"pages": ${pages(index).mkString("[", ",\n", "]")}};
     """.stripMargin
  }

  def pages(index: Index): Seq[String] =
    index.entries.map(page)

  def page(entry: IndexEntry): String =
    s"""{"title":"${entry.title}", "text":"${entry.text.replace("\"", "\\\"").replace("\n", "")}", "tags":${entry.tags.mkString("\""," ", "\"")}, "loc":"${entry.path.path}"}"""

  implicit def IndexMonoid: Monoid[Index] = new Monoid[Index] {
    def zero = Index(Vector())
    def append(a: Index, b: =>Index) = Index(a.entries ++ b.entries)
  }

  val reducer = Reducer.unitReducer((page: IndexedPage) => Index(Indexing.createEntries(page)))
}

case class IndexEntry(title: String, text: String, tags: Vector[String], path: FilePath)

