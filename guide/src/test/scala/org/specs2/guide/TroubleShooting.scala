package org.specs2
package guide

object TroubleShooting extends UserGuidePage { def is = s2"""

This section presents some of the common pitfalls you might face when using $specs2 and Scala

### vals vs lazy vals

The common symptom here is a `NullPointerException` for some attributes of your specification. You can refer to this [link]() for a detailed explanation of variable initialisation in Scala.
In the meantime the basic fix is to use a `lazy val` instead of a `val`.

### Lost expectations

You might expect the following specification to fail:${snippet{
class ShouldItFail extends Specification { def is = s2"""
  Should this example fail? $e1
"""
  def e1 = {
    1 must_== 100000 // do you expect this to fail
    10 must_== 10
  }
}
}}

${Structure}
lost expectatinos in Acceptance specs -> use a warning for expression as statements

### AsResult

### Inference

multiple statements a must be bla; b must be blo

"""
}

