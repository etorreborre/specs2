package org.specs2
package specification

import execute._
import org.specs2.matcher.Matcher
import org.specs2.specification.dsl.Online
import scalaz.stream.Process
import reporter.TextPrinterSpec._
import org.specs2.specification.core.{Fragments, FragmentsContinuation, Execution, Text}

class OnlineSpecificationSpec extends Specification { def is = s2"""

 A specification can have examples returning a result and Fragments depending on the result value $e1

"""
  val factory = fragmentFactory; import factory._

  def e1 = {
    def continue(n: Int): FragmentsContinuation = FragmentsContinuation { r: Result =>
      if (n == 1) None
      else        Some(core.Fragments(Process.emit(break) fby createExample(n - 1).contents))
    }

    def online(n: Int) = Execution(success, continue(n))

    def createExample(n: Int) = core.Fragments(fragmentFactory.example(Text("an online example"), online(n)))

    createExample(3) contains
      """|[info] + an online example
         |[info] + an online example
         |[info] + an online example""".stripMargin
  }

}

class WikipediaBddSpec extends Specification with Online { def is = s2"""
 All the pages mentioning the term BDD must contain a reference to specs2 $e1
"""

  def e1 = {
    val pages = Wikipedia.getPages("BDD")

    { pages must contain((_:Page) must mention("specs2")) } continueWith
      pagesSpec(pages)
  }

  def pagesSpec(pages: Seq[Page]): Fragments = {
    val specs2Links = pages.flatMap(_.getLinks).filter(_.getName.contains("specs2"))

    s2"""

 The specs2 links must all contain a reference to the author of specs2
 ${Fragments.foreach(specs2Links)(authorExample)}
    """
  }

  def authorExample(link: HtmlLink) =
    s2"""
  The page at ${link.getName}
    contains the name torreborre ${ link.getLinkedPage must mention("torreborre") }"""

  def mention(name: String): Matcher[Page] = (page: Page) => (true, "ok")

  object Wikipedia {
    def getPages(searchTerm: String): Seq[Page] = Seq(new Page {}, new Page {})
  }

  trait Page {
    def getLinks: Seq[HtmlLink] = Seq(new HtmlLink { def getName = "specs2-1" }, new HtmlLink { def getName = "specs2-2" })
  }

  trait HtmlLink {
    def getName: String
    def getLinkedPage: Page = new Page {}
  }
}
