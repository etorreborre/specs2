package org.specs2
package html

import mutable.Spec
import HtmlTemplate._
import matcher.ActionMatchers._

class HtmlTemplateSpec extends Spec {
  "replace variables in a template" >> {
    runTemplate(
      "hello $name$, I'm $me$ and this is not defined $undefined$",
      Map("name" -> "you",
          "me"   -> "eric")) must
    beOkWithValue("hello you, I'm eric and this is not defined ")
  }

  "$$ is how $ is quoted and should be replaced by $" >> {
    runTemplate(
      "hello $$name",
      Map("name" -> "eric")) must
      beOkWithValue("hello $name")
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
