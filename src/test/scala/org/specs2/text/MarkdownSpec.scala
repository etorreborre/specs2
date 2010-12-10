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
                                                                                          end
  
}