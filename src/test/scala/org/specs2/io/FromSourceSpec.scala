package org.specs2
package io
import mutable._
import specification.{SpecificationStructure, Example}
import user.specification.DifferentSpecification

class FromSourceSpec extends Specification with FromSource {
  val spec = new user.specification.UserFromSourceSpecification

  "An expression can be read from a source file" in {
    examples(spec)(0).desc.toString must contain("1 must_== 1")
  }
  "An expression can be read from a source file even if it spans several lines" in {
    examples(spec)(1).desc.toString must contain("val a") and contain("hello world")
  }
  "Even if the specification name is different from the file name" in {
    examples(new DifferentSpecification)(0).desc.toString must contain("1 must_== 1")
  }
  "If the file is not found, the full path is shown to the user" in {
    other.NotFound.result.toString must be_==("No source file found at src/test/scala/org/specs2/io/other/FromSourceSpec.scala")
  }
  "If the specification doesn't end with an end fragment, the last example description should be found" in {
    examples(spec)(2).desc.toString must contain("a call to an example")
  }
  def examples(s: SpecificationStructure) = s.is.examples
}

package other {
  object NotFound extends org.specs2.Specification { def is = ""
    def result = success.desc
  }

}
