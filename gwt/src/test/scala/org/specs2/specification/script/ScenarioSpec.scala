package org.specs2.specification.script

class ScenarioSpec extends org.specs2.Specification with GWT with StandardDelimitedStepParsers { def is = s2"""

 Tests of a simple scenario ${scenario.start}
   Given a first name {Eric}
   And a last name {$torreborre}
   When we add them with {+}
   Then should get {Eric Torreborre} ${scenario.end}

"""

  val torreborre = "Torreborre"

  val scenario =
    Scenario("basic")
      .given(aString)
      .given(aString)
      .when(aString) {
        case operator :: last :: first :: _ =>
          s"$first $last"
      }
      .andThen(aString) { case expected :: actual :: _ =>
        actual must_== expected
      }

}
