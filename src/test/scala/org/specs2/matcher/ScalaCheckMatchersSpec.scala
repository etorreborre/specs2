package org.specs2
package matcher
import execute._
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import io._
import specification._

class ScalaCheckMatchersSpec extends SpecificationWithJUnit with ScalaCheck with ScalaCheckProperties with MockOutput {
  def is = 
"  A ScalaCheck property can be used in the body of an Example"                    ^	  
"    if it is proved the execution will yield a Success"                           ! prop1^
"    if it is a function which is always true, it will yield a Success"            ! prop2^
"    if it is a function which is always false, it will yield a Failure"           ! prop3^
"    if it is a property throwing an exception, it will yield an Error"            ! prop4^
                                                                                   p^
"  A specs2 matcher can be returned by a function to be checked with ScalaCheck"   ^	  
"    if it is a MatchSuccess the execution will yield a Success"                   ! matcher1^
"    if the type of the input parameter is not the same as the MatchResult type"   ^
"      it should still work"                                                       ! matcher2^
"      another way is to transform it as a property with .forAll"                  ! matcher3^
                                                                                   p^
"  A partial function can also be used in the body of the Example"                 ! partial1^
                                                                                   p^
"  A ScalaCheck property will create a result"                                     ^	  
"    with a number of expectations that is equal to the minTestsOk"                ! result1^
                                                                                   p^
"  It is possible to change the default parameters used for the test"              ^	  
"    by setting up new implicit parameters locally"                                ! config1^
                                                                                   p^
"  It is possible to display"                                                      ^	  
"    the executed tests by setting up display parameters locally"                  ! c(config2)^
"    the labels that are set on properties"                                        ! c(config3)^
                                                                                   end

  case object c extends Before { def before = clear() }
  
  val success100tries = Success("The property passed without any counter-example after 100 tries")

  def prop1 = ("example" ! proved).execute must_== 
	            Success("The property passed without any counter-example after 1 try")
  def prop2 = ("example" ! trueStringFunction.forAll).execute must_== success100tries
  def prop3 = ("example" ! identityFunction.forAll).execute.message must startWith("A counter-example is 'false'")
  def prop4 = ("example" ! exceptionProp).execute.toString must startWith("Error(A counter-example is")

  def partial1 = ("example" ! partialFunction.forAll).execute must_== success100tries
  def matcher1 = ("example" ! alwaysTrueWithMatcher).execute must_== success100tries
  def matcher2 = ("example" ! check(stringToBooleanMatcher)).execute must_== success100tries
  def matcher3 = ("example" ! stringToBooleanMatcher.forAll).execute must_== success100tries
  def result1 = ("example" ! trueFunction.forAll).execute.expectationsNb must_== 100

  implicit def params = display(minTestsOk -> 20)
  def config1 = ("example" ! trueFunction.forAll).execute.expectationsNb must_== 20
  def config2 = {
	  ("example" ! trueFunction.forAll).execute
	  messages.mkString must contain("passed 20 tests")
  }
  def config3 = {
	  ("example" ! (falseFunction.forAll :| "my property")).execute
	  messages.mkString must contain("my property")
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
