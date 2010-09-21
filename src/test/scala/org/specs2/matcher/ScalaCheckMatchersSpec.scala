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
"    if it is proved the execution will yield a Success" ! c(e1)^
"    if it is a function which is always true, it will yield a Success" ! c(e2)^
"    if it is a function which is always false, it will yield a Failure" ! c(e3)^
"    if it is a property throwing an exception, it will yield an Error" ! c(e4)^
p^
"  A ScalaCheck property will create a result"^	  
"    with a number of expectations that equal to the minTestsOk" ! c(e5)^
p^
"  It is possible to change the default parameters used for the test"^	  
"    by setting up new implicit parameters locally" ! c(e6)^
p^
"  It is possible to display the executed tests"^	  
"    by setting up display parameters locally" ! c(e7)^
end

  case object c extends Before {
	def before = clear()
  }
  def e1 = ("example" ! proved).execute must_== 
	       Success("The property passed without any counter-example after 1 try")
  def e2 = ("example" ! trueStringFunction.forAll).execute must_== 
	       Success("The property passed without any counter-example after 100 tries")
  def e3 = ("example" ! identityFunction.forAll).execute.message must startWith(
	       "A counter-example is 'false'")
  def e4 = ("example" ! exceptionProp).execute.toString must startWith("Error(A counter-example is")

  def e5 = ("example" ! alwaysTrueFunction.forAll).execute.expectationsNb must_== 100

  implicit def params = display(minTestsOk -> 20)
  def e6 = {
	("example" ! alwaysTrueFunction.forAll).execute.expectationsNb must_== 20
  }

  def e7 = {
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
  val identityAssert: Boolean => MatchResult[Boolean] = ((x: Boolean) => x must_== true)
  def exceptionProp = forAll((b: Boolean) => {throw new java.lang.Exception("boom"); true})
}
