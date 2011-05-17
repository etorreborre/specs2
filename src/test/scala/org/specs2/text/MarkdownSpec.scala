package org.specs2
package text
import Markdown._
import execute._

class MarkdownSpec extends Specification { def is =
                                                                                                                        """
  Markdown text can be transformed to an html equivalent.
                                                                                                                        """^
                                                                                                                        p^
  "Emphasized text"                                                                                                     ^
  { toHtmlNoPar("_hello_") must_== "<em>hello</em>" }                                                                   ^
                                                                                                                        p^
  "Bold-italics text"                                                                                                   ^
  { toHtmlNoPar("***hello***") must_== "<strong><em>hello</em></strong>" }                                              ^
                                                                                                                        p^
  "Multi-line text must preserve newlines"                                                                              ^
  { toHtmlNoPar("hello\nworld") must contain("hello<br/>world") }                                                       ^
                                                                                                                        p^
  "Embedded code"                                                                                                       ! e1^
  "Code with newlines must be enclosed in one code tag only"                                                            ! e2^
                                                                                                                        end

  val someCode = """
This is a paragraph presenting some code:

 * with a bullet point

        import org.specs2._
        Console.println("Hello world")

 * and another one

and no more code here"""

  def e1 = toHtmlNoPar(someCode) must contain("<code class='prettyprint'>")
  def e2 = toHtmlNoPar(someCode).split(" ").filter(_.trim.contains("</code>")) must have size(1)
}