package org.specs2.text

import Markdown.*
import org.specs2.main.Arguments
import org.specs2.mutable.*

class MarkdownSpec extends Spec:
  given Arguments = Arguments()

  "emphasized text" >>
  { toHtmlNoPar("_hello_") must ===("<em>hello</em>") }

  "mold-italics text" >>
  { toHtmlNoPar("***hello***") must ===("<strong><em>hello</em></strong>") }

  "multi-line text must preserve newlines" >>
  { toHtmlNoPar("hello\nworld") must contain("hello<br />\nworld") }

  "title and line break" >>
  { toXhtml("### Title\nline1\n\nline2").toString must not(contain("### Title")) }

  "embedded code" >>
  { toHtmlNoPar(someCode) must contain("""<code class="prettyprint">""") }

  "code with newlines must be enclosed in one code tag only" >>
  { toHtmlNoPar(someCode).split(" ").filter(_.trim.contains("</code>")) must haveSize(1) }

  "inlined code must not have <pre> tags" >>
  { toHtmlNoPar("this is some `inlined` code") must contain("""this is some <code class="prettyprint">inlined</code> code""") }

  "the encoding must be ok with utf-8 characters" >>
  { toXhtml("⊛").toString must contain("⊛") }

  "the encoding must be ok with utf-8 characters" >>
  { toXhtml("⊛").toString must contain("⊛") }

  val someCode = """
This is a paragraph presenting some code:

 * with a bullet point

        import org.specs2.*
        Console.println("Hello world")

 * and another one

and no more code here"""
