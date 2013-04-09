package org.specs2
package text
import Trim._

class TrimSpec extends Specification { def is = s2"""
### Trait for trimming strings
 
  The *Trim* trait can be used to trim all sorts of strings.

  Trim lines and replace characters
  ${eg{ " hello\rworld\n  ".trimNewLines === "hello\rworld" }}
  ${eg{ " hello (world)  ".trimReplace("(" -> "[", ")" -> "]") === "hello [world]" }}
  ${eg{ " hello \\world  ".trimReplaceAll("h" -> "H", "w" -> "W") === "Hello \\World" }}

  Trim enclosing symbols
  ${eg{ " (hello world)  ".trimEnclosing("(", ")") === "hello world" }}
  ${eg{ " ( (hello world) )  ".trimEnclosing("(", ")") === "(hello world)" }}
  "but not if they're not enclosing
  ${eg{ "hello world)".trimEnclosing("(", ")") === "hello world)" }}

  Trim enclosing xml tags
  ${eg{"<p>hello</p>".trimEnclosingXmlTag("p") === "hello" }}
  ${eg{"<p a=\"2\">hello</p>".trimEnclosingXmlTag("p") === "hello" }}

  ${"Remove some groups" ! e1}

  Remove empty lines
  ${eg{ "hello\n    \nworld".removeEmptyLines === "hello\nworld" }}

  Remove the first match
  ${eg{ "hello world".removeFirst("(l)*o") === "he world" }}
  Remove the last match
  ${eg{ "hello world".removeLast("o.ld") === "hello w" }}

  Remove new lines
  ${eg{ "hello\n\r world".removeNewLines === "hello world" }}

  Split and trim
  ${eg{ "a,b,c".splitTrim(",").toSeq === Seq("a", "b", "c") }}
  ${eg{ "a, b , c".splitTrim(",").toSeq === Seq("a", "b", "c") }}
  ${eg{ "a,  ,c".splitTrim(",").toSeq === Seq("a", "c") }}

  string unless condition
    returns the string if the condition is false
    ${eg{ "hello" unless false must_== "hello" }}
    returns an empty string if the condition is true
    ${eg{ "hello" unless true must_== "" }}

  ${"Last block returns the last block when lines are separated by empty lines"  ! e2}

  With the Trim trait it is also possible to offset a multi-line string
    with a positive offset ${
      """hello
        |world""".stripMargin.offset(n = 2) ===
        """  hello
          |  world""".stripMargin
    }
    with a negative offset ${
      """   hello
        |   world""".stripMargin.offset(n = -2) ===
        """ hello
          | world""".stripMargin
    }
    with a negative offset which is more than existing${
      """   hello
        |   world""".stripMargin.offset(n = -20) ===
        """hello
          |world""".stripMargin
    }

"""

  def e1 = "<li><p>hello\ndear\nworld</p></li>".
           replaceAll("<p>((.|\n)*)</p>", (s: String) => s.replace("\n", "<br/>")) ===
           "<li><p>hello<br/>dear<br/>world</p></li>"

  def e2 = "hello\nworld\n   \nhow are\nyou\n".lastBlock must_== "how are\nyou"

}