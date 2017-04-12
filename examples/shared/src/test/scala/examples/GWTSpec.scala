package examples

import org.specs2._
import specification.script.{GWT, StandardRegexStepParsers}

/**
 * This specifications shows how to use the GWT trait to create Scenarios in the Given/When/Then style
 */
class GWTSpec extends Specification with GWT with StandardRegexStepParsers { def is = s2"""

 A given-when-then example for a calculator                       ${calculator1.start}
   Given the following number: 1
   And a second number: 2
   And a third number: 6
   When I use this operator: +
   Then I should get: 9
   And it should be >: 0                                          ${calculator1.end}

 Now with the multiplication                                      ${calculator2.start}
   Given the following number: 4
   And a second number: 5
   And a third number: 6
   When I use this operator: *
   Then I should get: 120
   And it should be >: 10
   But not should be >: 150                                       ${calculator2.end}

"""
  val anOperator = readAs(".*: (.)$").and((s: String) => s)

  val calculator1 =
    Scenario("calculator1")
      .given(anInt)
      .given(anInt)
      .given(anInt)
      .when(anOperator) { case op :: i :: j :: k :: _ => if (op == "+") i+j+k else i*j*k }
      .andThen(anInt)   { case expected :: sum :: _ => sum === expected }
      .andThen(anInt)   { case expected :: sum :: _ => sum must be_>(expected) }

  val calculator2 =
    calculator1.withTitle("calculator2").
    andThen(anInt) { case expected :: sum :: _ => sum must not(be_>(expected)) }

}
