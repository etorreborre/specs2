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
  val examples = 
"  A ScalaCheck property can be used in the body of an Example"^	  
"    if it is proved the execution will yield a Success" ! prop1^
"    if it is a function which is always true, it will yield a Success" ! prop2^
"    if it is a function which is always false, it will yield a Failure" ! prop3^
"    if it is a property throwing an exception, it will yield an Error" ! prop4^
p^
"  A specs2 matcher in a function to check with ScalaCheck"^	  
"    if it is a MatchSuccess the execution will yield a Success" ! matcher1^
p^
"  A ScalaCheck property will create a result"^	  
"    with a number of expectations that equal to the minTestsOk" ! result1^
p^
"  It is possible to change the default parameters used for the test"^	  
"    by setting up new implicit parameters locally" ! config1^
p^
"  It is possible to display the executed tests"^	  
"    by setting up display parameters locally" ! c(config2)^
end

  case object c extends Before {
	def before = clear()
  }
  def prop1 = ("example" ! proved).execute must_== 
	       Success("The property passed without any counter-example after 1 try")
  def prop2 = ("example" ! trueStringFunction.forAll).execute must_== 
	       Success("The property passed without any counter-example after 100 tries")
  def prop3 = ("example" ! identityFunction.forAll).execute.message must startWith(
	       "A counter-example is 'false'")
  def prop4 = ("example" ! exceptionProp).execute.toString must startWith("Error(A counter-example is")

  def matcher1 = ("example" ! alwaysTrueWithMatcher).execute must_==
	  	       Success("The property passed without any counter-example after 100 tries")

  def result1 = ("example" ! alwaysTrueFunction.forAll).execute.expectationsNb must_== 100

  implicit def params = display(minTestsOk -> 20)
  def config1 = {
	("example" ! alwaysTrueFunction.forAll).execute.expectationsNb must_== 20
  }

  def config2 = {
	("example" ! alwaysTrueFunction.forAll).execute
	messages.last.toString.trim must beMatching(".*passed 20 tests.*")
  }
}


trait ScalaCheckProperties {  this: Specification =>
  def identityFunction = (a:Boolean) => a
  val identityProp = forAll(identityFunction)
  val alwaysTrueProp = proved
  val alwaysTrueFunction: Boolean => Boolean = { a: Boolean => true }
  val alwaysTrue = Gen.value(true)
  val alwaysFalse = Gen.value(false)
  val random = Gen.oneOf(true, false)
  val exceptionValues = Gen(p => throw new java.lang.Exception("e"))
  val trueFunction = ((x: Boolean) => true)
  val trueStringFunction = ((x: String) => true)
  val partialFunction: PartialFunction[Boolean, Boolean] = { case (x: Boolean) => true }
  val falseFunction = ((x: Boolean) => false)
  val alwaysTrueWithMatcher: Boolean => MatchResult[Boolean] = ((x: Boolean) => true must_== true)
  def exceptionProp = forAll((b: Boolean) => {throw new java.lang.Exception("boom"); true})
}
