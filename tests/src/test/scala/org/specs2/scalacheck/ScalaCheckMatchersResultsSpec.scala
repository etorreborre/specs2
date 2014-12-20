package org.specs2
package scalacheck

import org.scalacheck.{Gen, Properties, Arbitrary, Prop}
import matcher._
import execute._
import org.scalacheck.Prop.{forAll}
import scalaz.{Tag, Order, @@, Equal}
import scalaz.std.anyVal.intInstance
import scalaz.syntax.tag._
import BrokenEqualInstances._

class ScalaCheckMatchersResultsSpec extends Specification with ScalaCheck with ResultMatchers with ReturnsSyntax { def is = s2"""

 Reporting for Props

 ${ check(Prop.passed) returns "OK, passed 100 tests." }
 ${ check(Prop.falsified) must beFailing("Falsified after 0 passed tests.") }
 ${ check(Prop.undecided) must beFailing(".*Gave up after only 0 passed tests. 101 tests were discarded.*") }
 when there is a conversion exception
 ${ check(exceptionPropOnConversion) must beFailing(".*failure.*") }

 Pending or skipped results in a Prop make the result pending or skipped
 ${ check(pendingProp) must bePending("the value is false") }
 ${ check(skippedProp) must beSkipped("the value is false") }

 A FailureException makes a Failure
 ${ check(failureExceptionProp) must beFailing(".*failure.*") }

 Other exceptions are reported as errors

 normal exception
 ${ check(exceptionProp()) must beError(".*Exception raised on property evaluation.*") }
 the exception class must be displayed
 ${ check(exceptionProp()) must beError(".*java.lang.IllegalArgumentException.*") }
 if the message is null the exception cause must be displayed
 ${ check(exceptionProp("null")) must beError(".*caused by java.lang.Exception: cause.*") }
 the stacktrace must be displayed
 ${ check(exceptionProp()) must beLike { case Error(m, ex) => ex.getStackTrace must not be empty } }

 Labelled properties are reported
 ${ check(complexProp) must beFailing(".*result sum.*") }

 Nested ScalaCheck properties must be labelled
 ${ check(equal.laws[Int @@ BrokenEqual]) must beFailing("(\\s|.)*equal(\\s|.)*") }

 Collected data is reported
 ${ check(prop((i: Int) => true)) returns "OK, passed 100 tests." }



"""

  def check(prop: Prop): Result =
    check(prop, Parameters())

  def exceptionWithCause(msg: String = "boom") = new java.lang.IllegalArgumentException(msg, new java.lang.Exception("cause"))
  def exceptionProp(msg: String = "boom") = forAll((b: Boolean) => {throw exceptionWithCause(msg); true})
  def exceptionPropOnConversion: Prop = forAll { (b: Boolean) => {throw new execute.FailureException(failure); Prop.passed} }

  def failureExceptionProp = forAll((b: Boolean) => {throw new execute.FailureException(failure); true})
  def pendingProp = forAll((b: Boolean) => b must beTrue.orPending)
  def skippedProp = forAll((b: Boolean) => b must beTrue.orSkip)

  val complexProp = forAll { (m: Int, n: Int) =>
      (m == m)     :| "result #1"    &&
      (n == n)     :| "result #2"    &&
      (m == n + m) :| "result sum"
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
  implicit def brokenEqual[A](implicit ordA: Order[A]): Equal[A @@ BrokenEqual] =
    Equal.equal((a1, a2) => ordA.lessThan(a1.unwrap, a2.unwrap))
  implicit def arbitrary[A](implicit arbA: Arbitrary[A]): Arbitrary[A @@ BrokenEqual] =
    Arbitrary(arbA.arbitrary.map(Tag.apply))
}
