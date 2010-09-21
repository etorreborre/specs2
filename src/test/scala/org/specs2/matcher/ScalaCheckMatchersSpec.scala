package org.specs2
package matcher
import execute._
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Prop._

class ScalaCheckMatchersSpec extends SpecificationWithJUnit with ScalaCheck with ScalaCheckProperties {
  val examples = 
"  A ScalaCheck property can be used in the body of an Example"^	  
"    if it is proved the execution will yield a Success" ! e1^
"    if it is a function which is always true, it will yield a Success" ! e2^
"    if it is a function which is always false, it will yield a Failure" ! e3^
"    if it is a property throwing an exception, it will yield an Error" ! e4^
end

  def e1 = ("example" ! proved).execute must_== 
	       Success("The property passed without any counter-example after 1 try")
  def e2 = ("example" ! trueStringFunction.forAll).execute must_== 
	       Success("The property passed without any counter-example after 100 tries")
  def e3 = ("example" ! identityFunction.forAll).execute.message must startWith(
	       "A counter-example is 'false'")
  def e4 = ("example" ! exceptionProperty).execute must throwA[java.lang.Exception]
	       
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
  val falseFunction = ((x: Boolean) => if (x) x)
  val identityAssert: Boolean => MatchResult[Boolean] = ((x: Boolean) => x must_== true)
  def exceptionProperty = {throw new java.lang.Exception("boom"); proved}
}
