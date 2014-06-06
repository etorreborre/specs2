package org.specs2
package html

import mutable.Specification
import HtmlTemplate._
import control._

class HtmlTemplateSpec extends Specification {
  "replace variables in a template" >> {
    runTemplate(
      "hello $name$, I'm $me$ and this is not defined $undefined$",
      Map("name" -> "you",
          "me"   -> "eric")) must
    beOkWithValue("hello you, I'm eric and this is not defined ")
  }
  
  "use conditional in a template" >> {
    runTemplate(
      "hello $if(defined)$$name$$else$$me$ $endif$!",
      Map("defined" -> "true",
          "name"    -> "you",
          "me"      -> "eric")) must
      beOkWithValue("hello you!")
  }

}
