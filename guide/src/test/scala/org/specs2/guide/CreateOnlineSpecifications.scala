package org.specs2
package guide

import matcher.Matcher
import org.specs2.specification.core.{Fragments, FragmentsContinuation, foreachInSequence}
import specification.dsl.Online
import FragmentsContinuation._

object CreateOnlineSpecifications extends UserGuidePage { def is = s2"""
 Most of the specifications we write are known up front because this is precisely what guides the construction of our systems. However in some systems data comes first and drives what we can do with the system. For example we want to check that:

  1. all the Wikipedia pages mentioning the term "BDD" are referencing $specs2
  2. if there is a $specs2 link on the page, the linked page must provide the name of the author

More precisely we want to create one example for `1.` and if it succeeds, create as many examples as there are links in `2.`. This can be done with the `org.specs2.specification.dsl.Online` trait and the `continueWith` method:: ${snippet{
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
     ${specs2Links.repeat(authorExample)}
    """
  }

  def authorExample(link: HtmlLink) =
    s2"""
  The page at ${link.getName}
    contains the name torreborre ${ link.getLinkedPage must mention("torreborre") }"""

  // implement this matcher
  def mention(name: String): Matcher[Page] = ???
// 8<----
  // fill in the definitions below
  object Wikipedia {
    def getPages(searchTerm: String): Seq[Page] = ???
  }

  trait Page {
    def getLinks: Seq[HtmlLink] = ???
  }

  trait HtmlLink {
    def getName: String = ???
    def getLinkedPage: Page = ???
  }
}
}}

In the specification above, if we succeed in checking each BDD page then we continue with the creation of individual examples for each encountered link.

"""
}
