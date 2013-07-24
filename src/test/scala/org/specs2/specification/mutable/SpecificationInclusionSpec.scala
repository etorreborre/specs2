package org.specs2
package specification
package mutable

import _root_.org.specs2.mutable.{Specification => Spec}
import main.CommandLineArguments
import reporter.SilentConsoleReporter

class SpecificationInclusionSpec extends Spec {

  "A specification can be included in another one" >> {
    val spec1 = new Spec { "spec1".title; "ex1" >> ok }
    val spec2 = new Spec { "spec2".title; "ex2" >> ok; include(spec1) }
    spec2.content.fragments.map(_.toString) === Seq(
      "SpecStart(spec2)", "Br()", "Br()",
      "Example(ex2)", "Br()",
      "SpecStart(spec1)", "Br()", "Br()",
      "Example(ex1)", "Br()",
      "SpecEnd(spec1)",
      "SpecEnd(spec2)"
    )
  }

  "A specification can be nested in another one" >> {
    val spec1 = new Spec { "spec1".title; "ex1" >> ok }
    val spec2 = new Spec { "spec2".title; "ex2" >> ok; inline(spec1) }
    spec2.content.fragments.map(_.toString) === Seq(
      "SpecStart(spec2)", "Br()", "Br()",
      "Example(ex2)", "Br()", "Br()", "Br()",
      "Example(ex1)", "Br()",
      "SpecEnd(spec2)"
    )
  }

  "A specification extending CommandLineArguments can be included in another one with the arguments coming from the parent specification" >> {
    val spec2 = new SpecInclusionSpec2
    spec2.set(xonly)
    !SilentConsoleReporter.report(spec2)(args()).hasIssues
  }
}

class SpecInclusionSpec1 extends Spec with CommandLineArguments { "spec1".title; "ex1" >> arguments.xonly }
class SpecInclusionSpec2 extends Spec with CommandLineArguments { "spec2".title; "ex2" >> ok; include(arguments, new SpecInclusionSpec1) }

