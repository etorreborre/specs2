package org.specs2
package specification

import execute.*
import core.*, Execution.*
import matcher.Matcher
import specification.dsl.Online
import reporter.TextPrinterSpecification
import control.*
import producer.*, Producer.*

class OnlineSpecificationSpec extends Specification with Online with OwnEnv {
  def is = s2"""

 A specification can have examples returning a result and Fragments depending on the result value $e1

"""
  val specification = TextPrinterSpecification(env)
  import specification.*

  val factory = fragmentFactory; import factory.*

  def online(n: Int): Execution =
    if n == 0 then NoExecution
    else success.continueWith(core.Fragments(oneAsync(break) `append` createExample(n - 1).contents))

  def createExample(n: Int): Fragments =
    "an online example" ! online(n)

  def e1 =
    createExample(3) `contains`
      """|[info] + an online example
         |[info] + an online example
         |[info] + an online example""".stripMargin

}

class WikipediaBddSpec extends Specification with Online {
  def is = s2"""
 All the pages mentioning the term BDD must contain a reference to specs2 $e1
"""

  def e1 =
    val pages = Wikipedia.getPages("BDD")

    { pages must contain((_: Page) must mention("specs2")) } `continueWith`
      pagesSpec(pages)

  def pagesSpec(pages: Seq[Page]): Fragments =
    val specs2Links = pages.flatMap(_.getLinks).filter(_.getName.contains("specs2"))

    s2"""

 The specs2 links must all contain a reference to the author of specs2
 ${Fragments.foreach(specs2Links)(authorExample)}
    """

  def authorExample(link: HtmlLink) =
    s2"""
  The page at ${link.getName}
    contains the name torreborre ${link.getLinkedPage must mention("torreborre")}"""

  def mention(name: String): Matcher[Page] = (page: Page) => (true, "ok")

  object Wikipedia:
    def getPages(searchTerm: String): Seq[Page] = Seq(new Page {}, new Page {})

  trait Page:
    def getLinks: Seq[HtmlLink] =
      Seq(new HtmlLink { def getName = "specs2-1" }, new HtmlLink { def getName = "specs2-2" })

  trait HtmlLink:
    def getName: String
    def getLinkedPage: Page = new Page {}
}
