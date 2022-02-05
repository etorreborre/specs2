package org.specs2
package html

import io.{DirectoryPath, FilePath, FileSystem}
import specification.core.*
import org.specs2.fp.*
import org.specs2.fp.syntax.*
import control.*
import org.specs2.concurrent.ExecutionEnv
import origami.*, Folds.*

/** Fold functions to create index files
  */
object Indexing:

  given ExecutionEnv =
    ExecutionEnv.fromGlobalExecutionContext

  /** An Index fold creates an Index page based on all pages to index and saves it to a given file path
    */
  def indexFold(path: FilePath): Fold[Operation, IndexedPage, Index] =
    fromMonoidMap[Operation, IndexedPage, Index](Index.createIndex).mapFlatten((index: Index) =>
      FileSystem(ConsoleLogger()).writeFile(path, Index.toJson(index)).as(index)
    )

  def createIndexedPages(env: Env, specifications: List[SpecStructure], outDir: DirectoryPath): List[IndexedPage] =
    specifications.map(createIndexedPage(env, outDir))

  def createIndexedPage(env: Env, outDir: DirectoryPath) = (spec: SpecStructure) => {
    IndexedPage(
      path = SpecHtmlPage.outputPath(outDir, spec).relativeTo(outDir),
      title = spec.header.showWords,
      contents = spec.textsList.foldLeft(new StringBuilder)((res, cur) => res.append(cur.description.show)).toString,
      tags = spec.tagsList.flatMap(_.names).toIndexedSeq
    )
  }

  def createEntries(page: IndexedPage): Vector[IndexEntry] =
    Vector(IndexEntry(page.title, page.contents, page.tags, page.path))

case class IndexedPage(path: FilePath, title: String, contents: String, tags: IndexedSeq[String])

case class Index(entries: Vector[IndexEntry]):
  def add(entry: IndexEntry) = copy(entries :+ entry)
  def add(other: Seq[IndexEntry]) = copy(entries ++ other)

object Index:

  val empty = Index(Vector())

  def toJson(index: Index): String =
    s"""
       |var tipuesearch = {'pages': ${pages(index).mkString("[", ",\n", "]")}};
     """.stripMargin

  def pages(index: Index): Seq[String] =
    index.entries.map(page)

  def page(entry: IndexEntry): String =
    s"""{'title':'${entry.title}', 'text':'${sanitizeEntry(entry)}', 'tags':${entry.tags.mkString(
        "'",
        " ",
        "'"
      )}, 'loc':'${entry.path.path}'}"""

  /** the text that is used for indexing must be sanitized:
    *   - no newlines
    *   - ^ (because tipue search doesn't like it)
    *   - no markdown characters
    */
  def sanitizeEntry(entry: IndexEntry): String =
    entry.text
      .replace("'", "")
      .replace("\n", "")
      .replace("^", "")
      .replace("#####", "")
      .replace("####", "")
      .replace("###", "")
      .replace("##", "")
      .replace("  * ", "")
      .replace("```", "")
      .replace("<s2>", "")
      .replace("</s2>", "")

  given Monoid[Index] with
    def zero = Index(Vector())
    def append(a: Index, b: =>Index) = Index(a.entries ++ b.entries)

  val createIndex = (page: IndexedPage) => Index(Indexing.createEntries(page))

case class IndexEntry(title: String, text: String, tags: IndexedSeq[String], path: FilePath)
