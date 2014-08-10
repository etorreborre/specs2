package org.specs2
package reporter

import data.Fold._
import io._
import matcher.ControlMatchers._
import reporter.Indexing._

import scalaz.Scalaz._
import scalaz._
import scalaz.stream._

class IndexingSpec extends Specification { def is = s2"""
 From the set of all the generated html pages we can generate an index and convert it to the tipue search format.

 An index is built from Html pages     $e1
 The index can be saved to a Json file $e2

"""

  def e1 = indexState(pages(0), reporter.Index.empty) must_==
           reporter.Index(Vector(IndexEntry(title = "page 1", text = "content1", tags = Vector(), path = FilePath("page1"))))

  def e2 = {
    val path = "target" / "test" / "IndexingSpec" | "index.js"
    runFold(Process.emitAll(pages), indexFold(path)).run

    val expected =
    s"""|var tipuesearch = {"pages": [{"title":page 1, "text":content1, "tags":Vector(), "loc":page1},
        |{"title":page 2, "text":content2, "tags":Vector(), "loc":page2}]}""".stripMargin

    FileSystem.readFile(path).map(_.trim) must beOk(===(expected))
  }

  val pages = Seq(IndexedPage(FilePath("page1"), "page 1", "content1"),
                  IndexedPage(FilePath("page2"), "page 2", "content2"))

}



/**
var tipuesearch = {"pages": [
     {"title": "Tipue Search, a site search engine jQuery plugin", "text": "Tipue Search is a site search engine jQuery plugin. Tipue Search is open source and released under the MIT License, which means it's free for both commercial and non-commercial use. Tipue Search is responsive and works on all reasonably modern browsers.", "tags": "JavaScript", "loc": "http://www.tipue.com/search"},
     {"title": "Tipue Search Static mode demo", "text": "This is a demo of Tipue Search Static mode.", "tags": "", "loc": "http://www.tipue.com/search/demos/static"},
     {"title": "Tipue Search Live mode demo", "text": "This is a demo of Tipue Search Live mode.", "tags": "", "loc": "http://www.tipue.com/search/demos/live"},
     {"title": "Tipue Search docs", "text": "If you haven't already done so, download Tipue Search. Copy the tipuesearch folder to your site.", "tags": "documentation", "loc": "http://www.tipue.com/search/docs"},
     {"title": "Tipue jQuery plugins Support", "text": "If you're stuck we offer a range of flexible support plans for our jQuery plugins.", "tags": "", "loc": "http://www.tipue.com/support"},
     {"title": "Tipue drop, a search suggestion box jQuery plugin", "text": "Tipue drop is a search suggestion box jQuery plugin. Tipue drop is open source and released under the MIT License, which means it's free for both commercial and non-commercial use. Tipue drop is responsive and works on all reasonably modern browsers.", "tags": "JavaScript", "loc": "http://www.tipue.com/drop"},
     {"title": "Tipr, a small and simple jQuery tooltip plugin", "text": "Tipr is a small and simple jQuery tooltip plugin. It's free and open source. Tipr displays attractive tooltips, and it's a shade under 3 KB, CSS included.", "tags": "JavaScript", "loc": "http://www.tipue.com/tipr"},
     {"title": "The Tipue blog", "text": "An occasional blog covering CSS, jQuery and web development.", "tags": "", "loc": "http://www.tipue.com/blog"},
     {"title": "Tipue jQuery plugins", "text": "The Tipue jQuery plugins are free, open source and responsive. We offer a range of flexible support plans for our jQuery plugins, including free.", "tags": "", "loc": "http://www.tipue.com/jquery"},
     {"title": "About Tipue", "text": "Tipue is a small web development studio based in North London. We've been around for over a decade. We design innovative add-ins, plugins, code and features with Perl, MySQL and jQuery.", "tags": "", "loc": "http://www.tipue.com/about"}
]};

  */
