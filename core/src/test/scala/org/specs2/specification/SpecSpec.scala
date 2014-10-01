package org.specs2

class SpecSpec extends Spec { def is = sequential ^ s2"""
  A Spec specification contains very few implicits $e1
  Many matchers can be used in a simple spec       $e2
"""

  def e1 = {
    import scala.reflect.runtime.universe._
    typeOf[Spec].members.filter(m => m.isImplicit && m.isMethod).map(_.name.toString).toSet must
      beEqualTo(Set(
        "theValue",                        // must expectations
        "appendToArguments",               // to add arguments
        "fragmentsAsSpecStructure",        // for def is = s2"... directly
        "fragmentIsInterpolatedFragment",  // to interpolate steps and actions
        "asResultIsInterpolatedFragment",  // to interpolate results
        "specificationInStringContext",    // for the s2 macro
        "matcherIsValueCheck",             // to use ValueChecks in contain matchers
        "functionIsValueCheck"
      ))
  }

  def e2 = {
    Seq(1, 2, 3) must contain(2)
    Some(1) must beSome(1)
  }
}
