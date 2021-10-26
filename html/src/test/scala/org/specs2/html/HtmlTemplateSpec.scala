package org.specs2
package html

import mutable.Spec
import HtmlTemplate.*
import matcher.OperationMatchers.*

class HtmlTemplateSpec extends Spec:
  "replace variables in a template" >> {
    runTemplate("hello $name$, I'm $me$ and this is not defined $undefined$", Map("name" -> "you", "me" -> "eric")) must
      beOk("hello you, I'm eric and this is not defined ")
  }

  "$$ is how $ is quoted and should be replaced by $" >> {
    runTemplate("hello $$name", Map("name" -> "eric")) must
      beOk("hello $name")
  }

  "use conditional in a template" >> {
    runTemplate(
      "hello $if(defined)$$name$$else$$me$ $endif$!",
      Map("defined" -> "true", "name" -> "you", "me" -> "eric")
    ) must beOk("hello you!")
  }
