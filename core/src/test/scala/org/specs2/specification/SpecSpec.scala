package org.specs2

class SpecSpec extends Spec { def is = sequential ^ s2"""
  A Spec specification contains very few implicits $e1
"""

  def e1 = {
    import scala.reflect.runtime.universe._
    typeOf[Spec].members.filter(m => m.isImplicit && m.isMethod).map(_.name.toString).toSet must
      beEqualTo(Set(
        "theValue",                        // must expectations
        "appendToArguments",               // to add arguments
        "fragmentsAsSpecStructure",        // for def is = s2"... directly
        "asResultIsInterpolatedFragment",  // to interpolate results
        "specificationInStringContext"     // for the s2 macro
      ))
  }
}
