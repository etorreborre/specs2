package org.specs2
package text
import Trim._

class TrimSpec extends Specification { def is =
                                                                                                                        """
### Trait for trimming strings
 
  The *Trim* trait can be used to trim all sorts of strings.
                                                                                                                        """^p^
  "Trim lines and replace characters"                                                                                   ^
  { " hello\rworld\n  ".trimNewLines === "hello\rworld" }                                                               ^
  { " hello (world)  ".trimReplace("(" -> "[", ")" -> "]") === "hello [world]" }                                        ^
  { " hello \\world  ".trimReplaceAll("h" -> "H", "w" -> "W") === "Hello \\World" }                                     ^
                                                                                                                        p^
  "Trim enclosing symbols"                                                                                              ^
  { " (hello world)  ".trimEnclosing("(", ")") === "hello world" }                                                      ^
  { " ( (hello world) )  ".trimEnclosing("(", ")") === "(hello world)" }                                                ^
  "but not if they're not enclosing"                                                                                    ^
  { "hello world)".trimEnclosing("(", ")") === "hello world)" }                                                         ^
                                                                                                                        endp^
  "Trim enclosing xml tags"                                                                                             ^
  { "<p>hello</p>".trimEnclosingXmlTag("p") === "hello" }                                                               ^
  { "<p a=\"2\">hello</p>".trimEnclosingXmlTag("p") === "hello" }                                                       ^
                                                                                                                        p^
  "Remove some groups"                                                                                                  ! e1^
                                                                                                                        p^
  "Remove empty lines"                                                                                                  ^
  { "hello\n    \nworld".removeEmptyLines === "hello\nworld" }                                                          ^
                                                                                                                        p^
  "Remove the first match"                                                                                              ^
  { "hello world".removeFirst("(l)*o") === "he world" }                                                                 ^
  "Remove the last match"                                                                                               ^
  { "hello world".removeLast("o.ld") === "hello w" }                                                                    ^
                                                                                                                        p^
  "Remove new lines"                                                                                                    ^
  { "hello\n\r world".removeNewLines === "hello world" }                                                                ^
                                                                                                                        p^
  "Split and trim"                                                                                                      ^
  { "a,b,c".splitTrim(",").toSeq === Seq("a", "b", "c") }                                                               ^
  { "a, b , c".splitTrim(",").toSeq === Seq("a", "b", "c") }                                                            ^
  { "a,  ,c".splitTrim(",").toSeq === Seq("a", "c") }                                                                   ^
                                                                                                                        p^
  "string unless condition"                                                                                             ^
    "returns the string if the condition is false"                                                                      ^
    { "hello" unless false must_== "hello" }                                                                            ^
    "returns an empty string if the condition is true"                                                                  ^bt^
    { "hello" unless true must_== "" }                                                                                  ^
                                                                                                                        endp^
  "Last block returns the last block when lines are separated by empty lines"                                           ! e2^
                                                                                                                        end

  def e1 = "<li><p>hello\ndear\nworld</p></li>".
           replaceAll("<p>((.|\n)*)</p>", (s: String) => s.replace("\n", "<br/>")) ===
           "<li><p>hello<br/>dear<br/>world</p></li>"

  def e2 = "hello\nworld\n   \nhow are\nyou\n".lastBlock must_== "how are\nyou"

}