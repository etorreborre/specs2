package org.specs2
package matcher
import execute._
import org.scalacheck._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import io._
import specification._

class ScalaCheckMatchersSpec extends SpecificationWithJUnit with ScalaCheck with ScalaCheckProperties with ResultMatchers { def is =

  "A ScalaCheck property can be used in the body of an Example"                                                         ^
    "if it is proved the execution will yield a Success"                                                                ! prop1^
    "if it is a function which is always true, it will yield a Success"                                                 ! prop2^
    "if it is a function which is always false, it will yield a Failure"                                                ! prop3^
    "if it is a property throwing an exception, it will yield an Error"                                                 ! prop4^
    "a Property can be used with checkProp"                                                                             ! prop5^
    "a Property can be used with check"                                                                                 ! prop6^
                                                                                                                        p^
  "A specs2 matcher can be returned by a function to be checked with ScalaCheck"                                        ^
    "if it is a MatchSuccess the execution will yield a Success"                                                        ! matcher1^
    "if the type of the input parameter is not the same as the MatchResult type"                                        ^
      "it should still work"                                                                                            ! matcher2^
      "another way is to transform it as a property with .forAll"                                                       ! matcher3^
                                                                                                                        p^
  "A partial function can also be used in the body of the Example"                                                      ! partial1^
  "A function with 2 parameters, returning a MatchResult be used in the body of the Example"                            ! partial2^
  "A function with 3 parameters, returning a MatchResult be used in the body of the Example"                            ! partial3^
  "A function with 4 parameters, returning a MatchResult be used in the body of the Example"                            ! partial4^
  "A function with 5 parameters, returning a MatchResult be used in the body of the Example"                            ! partial5^
                                                                                                                        p^
  "Arbitrary instances can be specified for a given property"                                                           ! check(arb1)^
                                                                                                                        p^
  "A ScalaCheck property will create a result"                                                                          ^
    "with a number of expectations that is equal to the minTestsOk"                                                     ! result1^
                                                                                                                        p^
  "It is possible to change the default parameters used for the test"                                                   ^
    "by setting up new implicit parameters locally"                                                                     ! config().e1^
                                                                                                                        p^
  "It is possible to display"                                                                                           ^
    "the executed tests by setting up display parameters locally"                                                       ! config().e2^
    "the labels that are set on properties"                                                                             ! config().e3^
                                                                                                                        end

  
  val success100tries = Success("The property passed without any counter-example after 100 tries")

  def prop1 = ("example" ! proved).execute must_== Success("The property passed without any counter-example after 1 try")
  def prop2 = ("example" ! trueStringFunction.forAll).execute must_== success100tries
  def prop3 = ("example" ! identityFunction.forAll).execute.message must startWith("A counter-example is 'false'")
  def prop4 = ("example" ! exceptionProp).execute.toString must startWith("Error(A counter-example is")
  def prop5 = ("example" ! checkProp(proved)).execute must beSuccessful
  def prop6 = ("example" ! check(proved)).execute must beSuccessful

  def partial1 = ("example" ! partialFunction.forAll).execute must_== success100tries
  def partial2 = {
    ("example" ! check { (s1: Boolean, s2: Boolean) =>
      s1 && s2 must_== s2 && s1
    }).execute must_== success100tries
  }
  def partial3 = {
    ("example" ! check { (s1: String, s2: String, s3: Int) =>
      1 must_== 1
    }).execute must_== success100tries
  }
  def partial4 = {
    ("example" ! check { (s1: String, s2: String, s3: Int, s4: Boolean) =>
      1 must_== 1
    }).execute must_== success100tries
  }
  def partial5 = {
    ("example" ! check { (s1: String, s2: String, s3: Int, s4: Boolean, s5: Double) =>
      1 must_== 1
    }).execute must_== success100tries
  }
  def arb1: Prop = {
    implicit def a = Arbitrary { for { a <- Gen.oneOf("a", "b"); b <- Gen.oneOf("a", "b") } yield a+b }
    (s: String) => s must contain("a") or contain("b")
  }

  def matcher1 = ("example" ! alwaysTrueWithMatcher).execute must_== success100tries
  def matcher2 = ("example" ! check(stringToBooleanMatcher)).execute must_== success100tries
  def matcher3 = ("example" ! stringToBooleanMatcher.forAll).execute must_== success100tries
  def result1 = ("example" ! trueFunction.forAll).execute.expectationsNb must_== 100

  case class config() extends Before with ScalaCheck with MockOutput {
    def before = clear()
    implicit def params = display(minTestsOk -> 20)
    def e1 = ("example" ! trueFunction.forAll).execute.expectationsNb must_== 20
    def e2 = {
  	  ("example" ! trueFunction.forAll).execute
  	  messages.mkString must contain("passed 20 tests")
    }
    def e3 = {
  	  ("example" ! (falseFunction.forAll :| "my property")).execute
  	  messages.mkString must contain("my property")
    }
  }
}


trait ScalaCheckProperties {  this: Specification =>
  def identityFunction = (a: Boolean) => a
  val trueFunction = (b: Boolean) => true
  val falseFunction = (b: Boolean) => false
  val trueStringFunction = (s: String) => true
  val partialFunction: PartialFunction[Boolean, Boolean] = { case (x: Boolean) => true }
  val alwaysTrueWithMatcher = (x: Boolean) => true must_== true
  val stringToBooleanMatcher = (x: String) => true must_== true
  val identityProp = forAll(identityFunction)
  val alwaysTrueProp = proved
  val alwaysTrue = Gen.value(true)
  val alwaysFalse = Gen.value(false)
  val random = Gen.oneOf(true, false)
  val exceptionValues = Gen(p => throw new java.lang.Exception("e"))
  def exceptionProp = forAll((b: Boolean) => {throw new java.lang.Exception("boom"); true})
}
