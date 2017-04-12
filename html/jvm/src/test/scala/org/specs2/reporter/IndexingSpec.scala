package org.specs2
package reporter

import io._
import matcher.OperationMatchers._
import org.specs2.html._
import Indexing._
import control._
import ExecuteActions._
import org.specs2.concurrent.ExecutionEnv

class IndexingSpec(implicit ee: ExecutionEnv) extends Specification { def is = s2"""
 From the set of all the generated html pages we can generate an index and convert it to the tipue search format.

 An index is built from Html pages     $index
 The index can be saved to a Json file $save
 Text must be quoted                   $quoted

"""

  def index = html.Index.createIndex(pages(0)) must_==
           html.Index(Vector(IndexEntry(title = "page 1", text = "content1", tags = Vector("tag1", "tag2"), path = FilePath("page1"))))

  def save = {
    val path = "target" / "test" / "IndexingSpec" | "index.js"
    runAction(emitAsync(pages:_*).fold(indexFold(path).into[Action]))(ee)

    val expected =
    s"""|var tipuesearch = {"pages": [{"title":"page 1", "text":"content1", "tags":"tag1 tag2", "loc":"page1"},
        |{"title":"page 2", "text":"content2", "tags":"tag3", "loc":"page2"}]};""".stripMargin

    FileSystem.readFile(path).map(_.trim) must beOk(===(expected))
  }

  def quoted =
    html.Index.page(IndexEntry("title", "text \"here\"", Vector(), FilePath("path"))) must contain("text \\\"here\\\"")

  val pages = Vector(IndexedPage(FilePath("page1"), "page 1", "content1", Vector("tag1", "tag2")),
                     IndexedPage(FilePath("page2"), "page 2", "content2", Vector("tag3")))

}

