package org.specs2
package scalacheck

import org.scalacheck.util.Pretty
import org.scalacheck.*
import matcher.*
import execute.*
import org.scalacheck.Prop.{forAll}
import org.specs2.main.CommandLine
import org.specs2.text.Trim.*

class ScalaCheckMatchersResultsSpec extends Specification with ScalaCheck with ResultMatchers with ReturnsSyntax {
  def is = s2"""

 Reporting for Props

   ${check(Prop.passed) returns "OK, passed 100 tests."}
   ${checkVerbose0(Prop.falsified) returns "The seed is"}
   ${check(Prop.falsified) must beFailing(withMessage("Falsified after 0 passed tests."))}
   ${check(Prop.undecided) must beFailing(withMessage("Gave up after only 0 passed tests. 501 tests were discarded"))}
   when there is a conversion exception
   ${check(exceptionPropOnConversion) must beFailing(withMessage("failure"))}

   Pending or skipped results in a Prop make the result pending or skipped
   ${check(pendingProp) must bePending(withMessage("the value is false"))}
   ${check(skippedProp) must beSkipped(withMessage("the value is false"))}

   A FailureException makes a Failure
   ${check(failureExceptionProp) must beFailing(withMessage("failure"))}
   ${check(propFailureExceptionProp) must beFailing(
    withMessage("Falsified after 1 passed tests.> ARG_0: true> failure")
  )}

   An AssertionError makes a Failure
   ${check(assertionErrorProp) must beFailing(withMessage("assertion failed"))}

   The stacktrace of a Failure is accessible
   ${check(failureWithStacktraceProp) must beLike { case Failure(_, _, st, _) =>
    st.map(_.getClassName) must
      contain((s: String) => s must contain("org.specs2.execute.Failure"))
  }}

   A failure with a datatable must report the datatable
   ${check(datatableFailureProp) must beFailing(withMessage("x \\| 1 \\| 2 \\| 1 != 2"))}

   A thrown datatable must report the datatable
   ${check(datatableThrownProp).message.trimLinesSpaceEnd ===
    """|Falsified after 0 passed tests.
       |> ARG_0: true
       |>
       |  | a | b |
       |x | 1 | 2 | ko
       |x | 1 | 2 | ko""".stripMargin}

 Other exceptions are reported as errors

   normal exception
   ${check(exceptionProp()) must beError(withMessage("Exception raised on property evaluation"))}
   the exception class must be displayed
   ${check(exceptionProp()) must beError(withMessage("java.lang.IllegalArgumentException"))}
   if the message is null the exception cause must be displayed
   ${check(exceptionProp("null")) must beError(withMessage("caused by java.lang.Exception: cause"))}
   the stacktrace must be displayed
   ${check(exceptionProp()) must beLike { case Error(m, ex) => ex.getStackTrace must not(beEmpty) }}

 Additional data

   Labelled properties are reported
   ${check(complexProp) must beFailing(withMessage("result sum"))}

   Nested ScalaCheck properties must be labelled
   uncomment for testing, otherwise there's a println for the seed
   $${ check(new Properties("equal") { property("commutativity") = Prop.falsified}) must beFailing(withMessage("equal")) }

   Collected data is reported
   ${check(prop((i: Int) => true).collect.verbose).expected must haveMessage("Collected test data")}

   Failing arguments are reported
   ${check(prop((i: Int, s: String) => i.toString == s).setGens(Gen.const(0), Gen.const("1"))).message must
    (contain("ARG_0: 0") and contain("ARG_1_ORIGINAL: \"1\""))}

   The freqmap instance is used to  report frequencies
   ${check(prop((i: Int) => true).prettyFreqMap(_ => "histogram").collect.verbose).expected must haveMessage(
    "histogram"
  )}

   Status is reported when parameters are set with display
   ${check(prop((i: Int) => true).display(minTestsOk = 10)).expected must haveMessage("OK, passed 10 tests")}

   Parameters can be passed from the command line
   ${check(
    prop { (i: Int, j: Int) => i === i }
      .setParameters(defaultParameters.overrideWith(CommandLine.create("scalacheck.mintestsok", "10")))
  ) returns "OK, passed 10 tests"}

   PrettyProduct better render case classes to replay examples
   ${check(prop((i: MyInt) => false)) returns """MyInt(1, "hey")"""}
"""

  def check(prop: ScalaCheckProperty): Result =
    check(prop.prop, prop.parameters.setVerbosity(-1), prop.prettyFreqMap)

  def checkVerbose0(prop: Prop): Result =
    check(prop, defaultParameters, defaultFreqMapPretty)

  def check(prop: Prop): Result =
    check(prop, defaultParameters.setVerbosity(-1), defaultFreqMapPretty)

  def exceptionWithCause(msg: String = "boom") =
    new java.lang.IllegalArgumentException(msg, new java.lang.Exception("cause"))
  def exceptionProp(msg: String = "boom") = forAll((b: Boolean) => { throw exceptionWithCause(msg); true })
  def exceptionPropOnConversion: Prop = forAll((b: Boolean) => { throw execute.FailureException(failure); Prop.passed })

  def failureExceptionProp = forAll((b: Boolean) => { throw execute.FailureException(failure); true })

  // only throw on the second try to make sure the reporting is showing the right tests number
  // this also reproduces what happens when ThrownExceptions is used with ScalaCheck
  var doneOnce = false
  def propFailureExceptionProp =
    scalaCheckPropertyAsResult.asResult {
      prop { (b: Boolean) =>
        if !doneOnce then doneOnce = true
        else throw new execute.FailureException(failure)
        true
      }.setGen(Gen.const(true)).setVerbosity(-1)
    }

  def assertionErrorProp = forAll((b: Boolean) => { assert(1 == 2, "1 is not equal to 2"); true })

  def failureWithStacktraceProp = forAll((b: Boolean) => 1 must ===(2))

  import DataTables.{given}

  def datatableFailureProp = forAll { (b: Boolean) =>
    "a" | "b" |>
      1 ! 1 |
      1 ! 2 | { (a, b) => a must ===(b) }
  }

  def datatableThrownProp = forAll(Gen.const(true)) { (b: Boolean) =>
    "a" | "b" |>
      1 ! 2 |
      1 ! 2 | { (a, b) =>
        throw new FailureException(failure("ko")); ok
      }
  }

  def pendingProp = forAll((b: Boolean) => b must beTrue.orPending)
  def skippedProp = forAll((b: Boolean) => b must beTrue.orSkip)

  val complexProp = forAll { (m: Int, n: Int) =>
    (m == m) :| "result #1" &&
    (n == n) :| "result #2" &&
    (m == n + m) :| "result sum"
  }

  def haveMessage(m: String) = withMessage(m)

  def withMessage(m: String) =
    beMatching(s".*$m.*") ^^ ((_: String).replace("\n", ""))

  case class MyInt(i: Int, s: String = "hey")

  object MyInt:
    given Arbitrary[MyInt] = Arbitrary(Gen.const(MyInt(1)))
    given (MyInt => Pretty) = PrettyProduct[MyInt]
}

class TSpec extends mutable.Specification with ScalaCheck:
  "a prop" >> prop { (i: Int) =>
    true
  }

class SeedSpec extends Specification with ScalaCheck:
  def is = sequential ^ s2"""
  A seed can be set on a property $runProperty
  The generated values must be different $checkValues
  """

  var generated: List[(Int, Int)] = List()

  def runProperty = prop { (x: Int, y: Int) =>
    generated = (x, y) +: generated
  }.setSeed("5dHu0rwf1jZ22C-BHl3poKhOY8iXY19a9jdB0JL6ZIJ=")

  def checkValues =
    // we expected at least 50 different generated values
    generated.distinct.size.pp must be_>=(50)
