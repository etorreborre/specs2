package org.specs2
package text
import Trim._

class TrimSpec extends SpecificationWithJUnit { def is = 
                                                                                          """
### Trait for trimming strings
 
  The *Trim* trait can be used to trim all sorts of strings.
                                                                                          """^
                                                                                          p^
  "Trim lines and replace characters"                                                     ^
  { " hello\rworld\n  ".trimNewLines === "hello\rworld" }                                   ^
  { " hello (world)  ".trimReplace("(" -> "[", ")" -> "]") === "hello [world]" }          ^
  { " hello world  ".trimReplaceAll("h" -> "H", "w" -> "W") === "Hello World" }           ^
                                                                                          p^
  "Trim enclosing symbols"                                                                ^
  { " (hello world)  ".trimEnclosing("(", ")") === "hello world" }                        ^
  { " ( (hello world) )  ".trimEnclosing("(", ")") === "(hello world)" }                  ^
                                                                                          p^
  "Trim enclosing xml tags"                                                               ^
  { "<p>hello</p>".trimEnclosingXmlTag("p") === "hello" }                                 ^
  { "<p a=\"2\">hello</p>".trimEnclosingXmlTag("p") === "hello" }                         ^
                                                                                          p^
  "Remove some groups"                                                                    !e1^
                                                                                          end
  def e1 = "<li><p>hello\ndear\nworld</p></li>".
           replaceAll("<p>((.|\n)*)</p>", (s: String) => s.replace("\n", "<br/>")) ===
           "<li><p>hello<br/>dear<br/>world</p></li>"
}