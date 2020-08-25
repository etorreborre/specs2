package org.specs2
package text

import org.specs2.text.Regexes._

class RegexesSpec extends mutable.Spec:

  "`matchesSafely` can be used to match with string which might be malformed regular expression" >> {
    "if the expression is well formed" >> {
      "eric" matchesSafely ".*r.c"
    }

    "if the expression is not well formed" >> {
      "][" matchesSafely "]["
    }
  }

  "`matchesSafely` can match special characters" >> {
    "if the expression contains a newline" >> {
      "a\nb" matchesSafely "a\nb"
    }

    "if the expression contains a tab" >> {
      "a\tb" matchesSafely "a\tb"
    }

    "if the expression contains a carriage return" >> {
      "a\rb" matchesSafely "a\rb"
    }
  }


