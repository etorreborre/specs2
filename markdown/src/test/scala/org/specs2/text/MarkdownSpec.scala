package org.specs2.text

import Markdown._
import org.specs2.main.Arguments
import org.specs2.mutable._

class MarkdownSpec extends Specification {
  implicit val defaultArgs = Arguments()

  "Emphasized text" >>
  { toHtmlNoPar("_hello_") must_== "<em>hello</em>" }
  "Bold-italics text" >>
  { toHtmlNoPar("***hello***") must_== "<strong><em>hello</em></strong>" }
  "Multi-line text must preserve newlines" >>
  { toHtmlNoPar("hello\nworld") must contain("hello<br/>world") }

  "title and line break" >>
  { toXhtml("### Title\nline1\n\nline2").toString must not contain("### Title") }


  "Embedded code" >>
  { toHtmlNoPar(someCode) must contain("""<code class="prettyprint">""") }
  "Code with newlines must be enclosed in one code tag only" >>
  { toHtmlNoPar(someCode).split(" ").filter(_.trim.contains("</code>")) must have size(1) }

  "the encoding must be ok with utf-8 characters" >>
  { toXhtml("⊛").toString must contain("⊛") }
  "the encoding must be ok with utf-8 characters" >>
  { toXhtml("⊛").toString must contain("⊛") }

  "verbatim code blocks are activated by default" >> {
    toHtml("""
             |     this is some text
           """.stripMargin, MarkdownOptions(verbatim = false)) must not contain("code")
  }
  "verbatim code blocks can alse be rendered as simple text" >> {
    toHtml("""
             |     this is some text
           """.stripMargin, MarkdownOptions(verbatim = false)) must not contain("code")
  }

  val someCode = """
This is a paragraph presenting some code:

 * with a bullet point

        import org.specs2._
        Console.println("Hello world")

 * and another one

and no more code here"""

}