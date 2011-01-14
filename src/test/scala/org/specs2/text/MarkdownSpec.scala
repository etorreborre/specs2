package org.specs2
package text
import Markdown._

class MarkdownSpec extends SpecificationWithJUnit { def is =
                                                                                          """
  Markdown text can be transformed to an html equivalent.
                                                                                          """^
                                                                                          p^
  "Emphasized text"                                                                       ^
  { toHtmlNoPar("_hello_") must_== "<em>hello</em>" }                                     ^
                                                                                          p^
  "Bold-italics text"                                                                     ^
  { toHtmlNoPar("***hello***") must_== "<strong><em>hello</em></strong>" }                ^
                                                                                          p^
  "Multi-line text must preserve newlines"                                                ^
  { toHtmlNoPar("hello\nworld") must contain("hello<br/>\nworld") }                            ^
                                                                                          p^
  "Embedded code"                                                                         ! e1^
                                                                                          end

  val someCode = """
This is a paragraph presenting some code:

    import org.specs2._
    Console.println("Hello world")

and no more code here"""

  def e1 = toHtmlNoPar(someCode) must contain("<pre>") and contain("<code class='prettyprint'>")
}