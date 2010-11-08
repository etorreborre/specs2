package org.specs2
package text
import Trim._

class TrimSpec extends SpecificationWithJUnit { def is = 
  
  "Trim lines and characters"                                                             ^ 
  { " hello\rworld\n  ".trimNewLines === "helloworld" }                                   ^
  { " hello (world)  ".trimReplace("(" -> "[", ")" -> "]") === "hello [world]" }          ^
  { " hello world  ".trimReplaceAll("h" -> "H", "w" -> "W") === "Hello World" }           ^
                                                                                          end^p^
  "Trim enclosing"                                                                        ^ 
  { " (hello world)  ".trimEnclosing("(", ")") === "hello world" }                        ^
  { " ( (hello world) )  ".trimEnclosing("(", ")") === " (hello world) " }                ^
                                                                                          end
}