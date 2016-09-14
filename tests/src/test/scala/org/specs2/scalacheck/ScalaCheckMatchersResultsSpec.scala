package org.specs2
package scalacheck

import org.scalacheck.util.Pretty
import org.scalacheck.{Gen, Properties, Arbitrary, Prop}
import matcher._
import execute._
import org.scalacheck.Prop.{forAll}
import org.specs2.main.CommandLine
import scalaz.{Tag, Order, @@, Equal, Id}
import scalaz.std.anyVal.intInstance
import BrokenEqualInstances._

class ScalaCheckMatchersResultsSpec extends Specification with ScalaCheck with ResultMatchers with ReturnsSyntax { def is = s2"""

 Reporting for Props

 ${ check(Prop.passed) returns "OK, passed 100 tests." }
 ${ check(Prop.falsified) must beFailing(withMessage("Falsified after 0 passed tests.")) }
 ${ check(Prop.undecided) must beFailing(withMessage("Gave up after only 0 passed tests. 501 tests were discarded")) }
 when there is a conversion exception
 ${ check(exceptionPropOnConversion) must beFailing(withMessage("failure")) }

 Pending or skipped results in a Prop make the result pending or skipped
 ${ check(pendingProp) must bePending(withMessage("the value is false")) }
 ${ check(skippedProp) must beSkipped(withMessage("the value is false")) }

 A FailureException makes a Failure
 ${ check(failureExceptionProp) must beFailing(withMessage("failure")) }

 The stacktrace of a Failure is accessible
 ${ check(failureWithStacktraceProp) must beLike { case Failure(_,_,st,_) => st.map(_.getClassName) must
    contain((s: String) => s must contain ("ScalaCheckMatchersResultsSpec")) } }

 A failure with a datatable must report the datatable
 ${check(datatableFailureProp) must beFailing(withMessage("x \\| 1 \\| 2 \\| 1 != 2"))}
 
 Other exceptions are reported as errors

 normal exception
 ${ check(exceptionProp()) must beError(withMessage("Exception raised on property evaluation")) }
 the exception class must be displayed
 ${ check(exceptionProp()) must beError(withMessage("java.lang.IllegalArgumentException")) }
 if the message is null the exception cause must be displayed
 ${ check(exceptionProp("null")) must beError(withMessage("caused by java.lang.Exception: cause")) }
 the stacktrace must be displayed
 ${ check(exceptionProp()) must beLike { case Error(m, ex) => ex.getStackTrace must not be empty } }

 Labelled properties are reported
 ${ check(complexProp) must beFailing(withMessage("result sum")) }

 Nested ScalaCheck properties must be labelled
 ${ check(equal.laws[Int @@ BrokenEqual]) must beFailing(withMessage("equal")) }

 Collected data is reported
 ${ check(prop((i: Int) => true).collect.verbose).expected must haveMessage("Collected test data") }

 Failing arguments are reported
 ${ check(prop((i: Int, s: String) => i.toString == s).setGens(Gen.const(0), Gen.const("1"))).message must
      (contain("ARG_0: 0") and contain("ARG_1: \"1\"")) }

 The freqmap instance is used to  report frequencies
 ${ check(prop((i: Int) => true).prettyFreqMap(_ => "histogram").collect.verbose).expected must haveMessage("histogram") }

 Status is reported when parameters are set with display
 ${ check(prop((i: Int) => true).display(minTestsOk = 10)).expected must haveMessage("OK, passed 10 tests") }


 Parameters can be passed from the command line
 ${ check(prop { (i: Int, j: Int) =>  i === i }.setParameters(defaultParameters.overrideWith(CommandLine.create("scalacheck.mintestsok", "10")))) returns "OK, passed 10 tests" }

 PrettyProduct better render case classes to replay examples
 ${ check(prop((i: MyInt) => false)) returns """MyInt(1, "hey")""" }

"""

  def check(prop: ScalaCheckProperty): Result =
    check(prop.prop, prop.parameters, prop.prettyFreqMap)

  def check(prop: Prop): Result =
    check(prop, defaultParameters, defaultFreqMapPretty)

  def exceptionWithCause(msg: String = "boom") = new java.lang.IllegalArgumentException(msg, new java.lang.Exception("cause"))
  def exceptionProp(msg: String = "boom") = forAll((b: Boolean) => {throw exceptionWithCause(msg); true})
  def exceptionPropOnConversion: Prop = forAll { (b: Boolean) => {throw new execute.FailureException(failure); Prop.passed} }

  def failureExceptionProp = forAll((b: Boolean) => {throw new execute.FailureException(failure); true})

  def failureWithStacktraceProp = forAll((b: Boolean) => 1 must_== 2)

  import DataTables._
  def datatableFailureProp = forAll { b: Boolean =>
    "a" | "b" |>
     1  ! 1   |
     1  ! 2   | { (a, b) => a must_== b }
   }

  def pendingProp = forAll((b: Boolean) => b must beTrue.orPending)
  def skippedProp = forAll((b: Boolean) => b must beTrue.orSkip)

  val complexProp = forAll { (m: Int, n: Int) =>
      (m == m)     :| "result #1"    &&
      (n == n)     :| "result #2"    &&
      (m == n + m) :| "result sum"
  }

  def haveMessage(m: String) = withMessage(m)

  def withMessage(m: String) =
    beMatching(s".*$m.*") ^^ ((_:String).replace("\n", ""))


  case class MyInt(i: Int, s: String = "hey")
  object MyInt {
    implicit def ArbInt: Arbitrary[MyInt] = Arbitrary(Gen.const(MyInt(1)))
    implicit def pretty: MyInt => Pretty = PrettyProduct[MyInt]
  }
}

object equal {
  def commutativity[A](implicit A: Equal[A], arb: Arbitrary[A]) = forAll(A.equalLaw.commutative _)
  def reflexive[A](implicit A: Equal[A], arb: Arbitrary[A])     = forAll(A.equalLaw.reflexive _)
  def transitive[A](implicit A: Equal[A], arb: Arbitrary[A])    = forAll(A.equalLaw.transitive _)
  def naturality[A](implicit A: Equal[A], arb: Arbitrary[A])    = forAll(A.equalLaw.naturality _)

  def laws[A](implicit A: Equal[A], arb: Arbitrary[A]) = new Properties("equal") {
    property("commutativity") = commutativity[A]
    property("reflexive")     = reflexive[A]
    property("transitive")    = transitive[A]
    property("naturality")    = naturality[A]
  }
}


sealed trait BrokenEqual
object BrokenEqualInstances {
  import scalaz.Id._

  implicit def brokenEqual[A](implicit ordA: Order[A]): Equal[A @@ BrokenEqual] =
    Equal.equal((a1, a2) => ordA.lessThan(Tag.unsubst[A, Id, BrokenEqual](a1), Tag.unsubst[A, Id, BrokenEqual](a2)))
  implicit def arbitrary[A](implicit arbA: Arbitrary[A]): Arbitrary[A @@ BrokenEqual] =
    Arbitrary(arbA.arbitrary.map(Tag.apply))
}

class TSpec extends mutable.Specification with ScalaCheck {
  "a prop" >> prop { i: Int =>
    true
  }
}
