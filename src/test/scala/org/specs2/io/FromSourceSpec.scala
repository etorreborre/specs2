package org.specs2
package io
import mutable._

class FromSourceSpec extends Specification with FromSource {
  
  "An expression can be read from a source file" in {
    (1 must_== 1).desc.toString must contain("(1 must_== 1)")
  }
  "Even if the specification name is different from the file name" in {
    DifferentSpecification.result.toString must contain("def result = success.desc")
  }
  "If the file is not found, the full path is shown to the user" in {
     other.NotFound.result.toString must be_==("No source file found at src/test/scala/org/specs2/io/other/FromSourceSpec.scala")
  }
}

object DifferentSpecification extends org.specs2.Specification { def is = ""
  def result = success.desc
}

package other {
  object NotFound extends org.specs2.Specification { def is = ""
    def result = success.desc
  }

}
