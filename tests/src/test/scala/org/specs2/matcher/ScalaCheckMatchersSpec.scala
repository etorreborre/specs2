package org.specs2
package matcher

import java.util

import org.scalacheck.Prop.{forAll, proved}
import org.scalacheck._
import execute._
import io._
import specification._
import specification.dsl.ExampleDsl
import specification.process._

import scala.sys.error

class ScalaCheckMatchersSpec extends Spec with ScalaCheckProperties { def is = s2"""

 A ScalaCheck property can be used in the body of an Example
   Here are some examples with
     a result
     ${ prop { (i:Int) => success } }

     a match result
     ${ prop { (i:Int) => i must be_>(0) or be_<=(0) } }

     a boolean value
     ${ prop { (i:Int) => i > 0 || i <= 0 } }

     a Prop
     ${ forAll { (i:Int) => i > 0 || i <= 0 } }

     an implication and a match result
     ${ prop { (i:Int) => (i > 0) ==> (i must be_>(0)) } }
     ${ prop { (i:Int, j: Int) => (i > j) ==> (i must be_>(j)) } }

     an implication and a boolean value
     ${ prop { (i:Int) => (i > 0) ==> (i > 0) } }

     a unit value with side-effects
     ${ prop { (i:Int) => { (1 to 5) foreach { n => n must_== n } } } }

     a specific arbitrary instance in the enclosing scope ${
       implicit val arbitrary = positiveInts
       prop { (i:Int) => i must be_>(0) }
     }

     a specific arbitrary instance
     ${ positiveInts { (i:Int) => i must be_>(0) } }

     several specific arbitrary instances
     ${ (positiveInts, positiveInts) { (i:Int, j: Int) => i+j must be_>(0) } }

     specific generation parameters
     ${ prop { (i:Int) => (i > 0) ==> (i > 0) } set (minTestsOk = 50) }

   if it is proved the execution will yield a Success                                                    $prop1
   if it is a function which is always true, it will yield a Success                                     $prop2
   if it is a function which is always false, it will yield a Failure                                    $prop3

   if it is a property throwing an exception
     it will yield an Error                                                                              $prop4
     showing the exception type if the message is null                                                   $prop4_1
     showing the cause                                                                                   $prop4_2
     showing the stacktrace                                                                              $prop4_3
   a Property can be used with check                                                                     $prop5
   a FailureException can be thrown from a Prop                                                          $prop6
   in the Context of a mutable specification                                                             $prop7
   with a Pending or Skipped result the Prop is undecided                                                $prop8
   if the Prop itself is an exception                                                                    $prop9

 It can also be used at the beginning of a specification                                                 $fragment1

 A specs2 matcher can be returned by a function to be checked with ScalaCheck
   if it is a MatchSuccess the execution will yield a Success                                            $matcher1
   if the type of the input parameter is not the same as the MatchResult type
     it should still work                                                                                $matcher2
     another way is to transform it as a property with .forAll                                           $matcher3

 A partial function can also be used in the body of the Example                                          $partial1
 A function with 2 parameters, returning a MatchResult be used in the body of the Example                $partial2
 A function with 3 parameters, returning a MatchResult be used in the body of the Example                $partial3
 A function with 4 parameters, returning a MatchResult be used in the body of the Example                $partial4
 A function with 5 parameters, returning a MatchResult be used in the body of the Example                $partial5

 Arbitrary instances can be specified for a given property                                               $arb1
 Gen instances can be used to define a property using matchers                                           $gen1

 A ScalaCheck property will create a Result
   with a number of expectations that is equal to the minTestsOk                                         $result1
   with one expectation per Prop if the OneExpectationPerProp trait is used                              $result2
   with failure details if any                                                                           $result3

 It is possible to change the default parameters used for the test
   by setting up new implicit parameters locally                                                         ${config().e1}

 Properties can be specified with no shrinking
  ${ propNoShrink { (i:Int) => i > 0 || i <= 0 } }
  ${ propNoShrink { (i:Int, j: Int) => (i > j) ==> (i must be_>(j)) } }

 It is possible to display
   the executed tests by setting up display parameters locally                                           ${config().e2}
   the labels that are set on properties                                                                 ${config().e3}
   the exceptions that happen on generation                                                              ${config().e4}
   the collected frequencies                                                                             ${config().e5}
                                                                                                         """

  
  val success100tries = Success("The property passed without any counter-example after 100 tries")

  def execute[R : AsResult](r: =>R): Result =
    DefaultExecutor.execute("example" ! r).executionResult

  def prop1 = execute(proved) must_== Success("The property passed without any counter-example after 1 try")
  def prop2 = execute(trueStringFunction.forAll) must_== success100tries
  def prop3 = execute(identityFunction.forAll).message must startWith("A counter-example is 'false'")

  def prop4   = execute(exceptionProp()).toString must startWith("Error(A counter-example is")
  def prop4_1 = execute(exceptionProp("null")).toString must contain("java.lang.Exception")
  def prop4_2 = execute(exceptionProp()).toString must contain("java.lang.IllegalArgumentException")
  def prop4_3 = execute(exceptionProp()) must beLike { case org.specs2.execute.Error(m, ex) => ex.getStackTrace must not be empty }

  def prop5 = execute(check(proved)) must beSuccessful
  def prop6 = execute(failureExceptionProp).toString must startWith("xA counter-example is")

  def prop7 = DefaultExecutor.runSpecification(new MutableSpecWithContextAndScalaCheck).map(_.executionResult).reduce(_ and _).isFailure
  def prop8 = execute(check(pendingProp)) must bePending
  def prop9 = execute(exceptionPropOnConversion).toString must startWith("A counter-example is")

  def fragment1 = {
    val spec = new Specification { def is = prop((i: Int) => i == i) }
    DefaultExecutor.runSpecification(spec).map(_.executionResult).reduce(_ and _).isSuccess
  }

  def partial1 = execute(partialFunction.forAll) must_== success100tries
  def partial2 = execute { prop { (s1: Boolean, s2: Boolean) => s1 && s2 must_== s2 && s1 } } must_== success100tries
  def partial3 = execute { prop { (s1: String, s2: String, s3: Int) => 1 must_== 1 } } must_== success100tries
  def partial4 = execute { prop { (s1: String, s2: String, s3: Int, s4: Boolean) => 1 must_== 1 } } must_== success100tries
  def partial5 = execute { prop { (s1: String, s2: String, s3: Int, s4: Boolean, s5: Double) => 1 must_== 1 } } must_== success100tries

  val positiveInts = Arbitrary(Gen.choose(1, 5))

  def arb1: Prop = {
    implicit def ab = Arbitrary { for { a <- Gen.oneOf("a", "b"); b <- Gen.oneOf("a", "b") } yield a+b }
    (s: String) => s must contain("a") or contain("b")
  }

  trait Side
  case class Left() extends Side
  case class Right() extends Side

  def gen1 = {
    def sides = Gen.oneOf(Left(), Right())
    forAll(sides) { (s: Side) => s must be_==(Left()) or be_==(Right()) }
  }

  def matcher1 = execute(prop(alwaysTrueWithMatcher)) must_== success100tries
  def matcher2 = execute(prop(stringToBooleanMatcher)) must_== success100tries
  def matcher3 = execute(prop(stringToBooleanMatcher)) must_== success100tries
  def result1  = execute(prop(trueFunction)).expectationsNb must_== 100
  def result2  = {
    val spec = new Specification with ScalaCheck with OneExpectationPerProp { def is = "test" ! prop(trueFunction) }
    spec.is.examples.map(_.executionResult).head.expectationsNb must_== 1
  }
  def result3  = execute(prop(failureWithDetails)) match {
    case f: org.specs2.execute.Failure => if (f.details == org.specs2.execute.NoDetails) ko("no details") else ok
    case _                             => ko("not a failure")
  }

  case class config() extends Before with ScalaCheckMatchers with StringOutput {
    def before = clear()
    def executionMessages(prop: Prop) = { execute(prop); messages.mkString }

    implicit def params = display(minTestsOk = 20)
    def e1 = execute(trueFunction.forAll).expectationsNb must_== 20
    def e2 = executionMessages(trueFunction.forAll) must contain("passed 20 tests")
    def e3 = executionMessages(falseFunction.forAll :| "my property") must contain("my property")
    def e4 = executionMessages(propertyWithGenerationException) must contain("boo")
    def e5 = executionMessages(propertyWithDataCollection) must contain("Collected test data")
  }
}


trait ScalaCheckProperties extends ScalaCheck with ResultMatchers with MustMatchers with ExampleDsl with StandardResults {
  def identityFunction = (a: Boolean) => a
  val trueFunction = (b: Boolean) => true
  val falseFunction = (b: Boolean) => false
  val trueStringFunction = (s: String) => true
  val partialFunction: PartialFunction[Boolean, Boolean] = { case (x: Boolean) => true }
  val alwaysTrueWithMatcher = (x: Boolean) => true must_== true
  val stringToBooleanMatcher = (x: String) => true must_== true
  val identityProp = forAll(identityFunction)
  val alwaysTrueProp = proved
  val alwaysTrue = Gen.const(true)
  val alwaysFalse = Gen.const(false)
  val random = Gen.oneOf(true, false)
  val failureWithDetails = (a: Boolean) => "abc" ==== "bca"

  val propertyWithGenerationException = {
    implicit def arb: Arbitrary[Int] = Arbitrary { for (n <- Gen.choose(1, 3)) yield { error("boo"); n }}
    Prop.forAll((i: Int) => i > 0)
  }
  val propertyWithDataCollection = Prop.forAll { l: List[Int] =>
    Prop.classify(l.reverse == l, "ordered") {
      Prop.classify(l.length > 5, "large", "small") {
        l.reverse.reverse == l
      }
    }
  }

  def exceptionWithCause(msg: String = "boom") = new java.lang.IllegalArgumentException(msg, new java.lang.Exception("cause"))
  def exceptionProp(msg: String = "boom") = forAll((b: Boolean) => {throw exceptionWithCause(msg); true})
  def exceptionPropOnConversion: Prop = prop { (b: Boolean) => {throw new execute.FailureException(failure); Prop.passed} }

  def failureExceptionProp = forAll((b: Boolean) => {throw new execute.FailureException(failure); true})
  def pendingProp = forAll((b: Boolean) => b must beTrue.orPending)
}

class MutableSpecWithContextAndScalaCheck extends org.specs2.mutable.Specification with ScalaCheck {
  "check something with before code" ! new SC {
    prop { (s: String) =>
      s.reverse must_== aString
    }.set(rng = new util.Random, minTestsOk = 200)
  }

  trait SC extends org.specs2.mutable.Before with StringOutput {
    val aString = "xxx"
    def before { println("before") }
  }
}
