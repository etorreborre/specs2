package org.specs2
package io

class FromSourceSpec extends SpecificationWithJUnit with FromSource { def is =
  
  "an expression can be read from a source file" ! {
    (1 must_== 1).desc.toString must contain("(1 must_== 1)")
  }^
  "if the file is not found, the full path is shown to the user" ! {
     NotFound.result.toString must be_==("No source file found at src/test/scala/org/specs2/io/NotFound.scala")
  }^
  end
}

object NotFound extends Specification { def is = ""
  def result = success.desc
}
