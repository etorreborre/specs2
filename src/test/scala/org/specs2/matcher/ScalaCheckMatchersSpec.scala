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
"    if it is always true the execution will yield a Success" ! e1

  def e1 = ("example" ! trueStringFunction.forAll).execute must_== 
	       Success("The property passed without any counter-example after 100 tries")
}


trait ScalaCheckProperties {  this: Specification =>
  val identityProp = forAll((a:Boolean) => a)
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
  val exceptionProperty = ((x: Boolean) => {throw new java.lang.Exception("e"); proved})
}
