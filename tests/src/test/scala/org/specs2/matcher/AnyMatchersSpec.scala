package org.specs2
package matcher

import java.io._
import execute._
import specification._

class AnyMatchersSpec extends script.Specification with Groups with ResultMatchers with AnyMatchers with ValueChecks with TypecheckMatchers { def is = s2"""

  beTheSameAs checks if a value is eq to another one
  ${ aValue must beTheSameAs(aValue) }
  ${ "a" must not beTheSameAs("b") }

  be is an alias for beTheSameAs
  ${ aValue must be(aValue) }
  ${ "a" must not be("b") }

  be_==~ checks the equality of 2 objects, up to an implicit conversion
  ${ 1L must be_==~(1) }
  ${ 2L must not be_==~(1) }
  ${ (2L must be_==~(1)).message must contain("2 != 1") }

  beTrue matches true values
  ${ true must beTrue }
  ${ (false must beTrue).message must_==  "the value is false" }

  beFalse matches false values
  ${ false must beFalse }
  ${ (true must beFalse).message must_== "the value is true" }

  beLike matches objects against a pattern
  ${ List(1, 2) must beLike { case List(a, b) => ok } }
  ${ List(1, 2) must beLike { case List(a, b) => (a + b) must_== 3 } }
  + if the match succeeds but the condition after match fails, a precise failure message can be returned

  toSeq allows to transform a single matcher to a matcher checking a Seq
  ${ List(1, 2, 3) must ((be_===(_:Int)).toSeq)(Seq(1, 2, 3)) }

  toSet allows to transform a single matcher to a matcher checking a Set
  ${ Set(1, 2, 3) must ((be_===(_:Int)).toSet)(Set(1, 2, 3)) }

 forall allows to transform a single matcher to a matcher checking that all elements of a Seq are matching
  ${ Seq(2, 3, 4) must contain(be_>=(2)).forall }
  ${ forall(Seq((1, 2), (3, 4))) { case (a, b) => a must be_<(b) } }
  ${ forallWhen(Seq((2, 1), (3, 4))) { case (a, b) if a > 2 => a must be_<(b) } }
  ${ (Seq(2, 3, 4) must contain(_:Int)).forall(Seq(2, 4)) }

  foreach is like forall but will execute all matchers and collect the results
  ${ Seq(2, 3, 4) must contain(be_>=(2)).foreach }
  ${ foreach(Seq((1, 2), (3, 4))) { case (a, b) => a must be_<(b) } }
  ${ foreachWhen(Seq((2, 1), (3, 4))) { case (a, b) if a > 2 => a must be_<(b) } }
  ${ ((_:Int) must be_>=(2)).foreach(Seq(2, 3, 4)) }
  if all expectations throws are Skipped then the whole result must be skipped $skipForeach

  atLeastOnce allows to transform a single matcher to a matcher checking that one element of a Seq is matching
  ${ Seq(2, 3, 4) must contain(be_>(2)).atLeastOnce }
  ${ ((_:Int) must be_>(2)).atLeastOnce(Seq(2, 3, 4)) }
  ${ ((i:Int) => MustExpectations.theValue(i) must be_>(2)).atLeastOnce(Seq(2, 3, 4)) }
  ${ atLeastOnce(Seq((4, 2), (3, 4))) { case (a, b) => a must be_<(b) } }
  ${ atLeastOnceWhen(Seq((2, 1), (3, 4))) { case (a, b) if a > 2 => a must be_<(b) } }
  ${ atLeastOnce(Seq(Some(1), None)) { _ must beSome(1) } }
  ${ (new org.specs2.mutable.Specification { atLeastOnce(Seq(1))(_ must be_<(0)) }) must throwA[FailureException] } ${tag("issue #169")}

  beNull matches null values
  ${ (null:String) must beNull }
  ${ (null:String) must be(null) }
  ${ "" must not beNull }
  ${ "" must not be null }

  beAsNullAs checks if two values are null at the same time
  ${ (null:String) must beAsNullAs(null) }
  ${ 1 must not be asNullAs(null) }
  ${ (null:String) must not be asNullAs(1) }
  ${ 1 must be asNullAs(1) }

  beOneOf matches a value is amongs others
  ${ 1 must beOneOf(1, 2, 3) }
  ${ 4 must not be oneOf(1, 2, 3) }

  haveClass checks if a value has a given class as its type
  ${ (1: java.lang.Integer) must haveClass[java.lang.Integer] }
  ${ BigInt(1) must not have klass[String] }

  haveSuperclass checks if a value has a given class as one of its ancestors
  ${ new BufferedInputStream(null) must haveSuperclass[InputStream] }
  ${ BigInt(1) must not have superClass[String] }

  haveInterface checks if a value has a given interface in the list of its interfaces
  ${ AsResult(new java.util.ArrayList() must haveInterface[java.util.List[_]]) }
  ${ AsResult(BigInt(1) must not have interface[java.util.List[_]]) }

  beAssignableFrom checks if a class is assignable from another
  ${ classOf[OutputStream] must beAssignableFrom[FileOutputStream] }

  beAnInstanceOf checks if an object is an instance of a given type
  ${ type1 must beAnInstanceOf[Type1] }
  ${ type1 must not be anInstanceOf[Type2] }
  ${ (type1 must beAnInstanceOf[Type2]).message must_== s"'type1: ${type1.getClass.getName}' is not an instance of 'org.specs2.matcher.Type2'" }
  // doesn't typecheck with AnyVals
  ${ Typecheck.typecheck("false must beAnInstanceOf[Boolean]") must not succeed }

Implicits
=========

  + the === implicits can be deactivated with the NoCanBeEqual trait
  + the must implicits can be deactivated with the NoMustExpectations trait
                                                                                                                        """
  "be like" - new group {
    eg := { (List(1, 2) must beLike { case List(a, b) => (a + b) must_== 2 }) returns "3 != 2" }
  }

  "must implicits" - new group {

    eg := {
      // if this specification compiles and if result is ok, this means that the === implicit could be redefined
      // thanks to the NoCanBeEqual trait
      val spec = new Specification with NoTypedEqual {
        implicit def otherTripleEqualUse[T](t: =>T) = new {
          def ===[S](other: S) = other
        }
        val result = (1 === 2) must_== 2
        def is = result
      }
      spec.result
    }

    eg := {
      // if this specification compiles and if result is ok, this means that the must implicit could be redefined
      // thanks to the NoMustExpectations trait
      val spec = new org.specs2.mutable.Specification with NoMustExpectations {
        implicit def aValue[T](t: =>T) = new {
          def must(other: Int) = other
        }
        val result = (1 must 2) === 2
        "an example" >> result
      }
      spec.result
    }
  }

  val aValue: String = "a value"

  val type1 = new Type1 { override def toString = "type1" }

  def skipForeach =
    { foreach(Seq(0, 1, 2)) { case a => a must be_<(0).orSkip("todo") } } must beLike { case MatchSkip(_,_) => ok }
}

trait Type1
trait Type2

case class Hello() { override def toString = "hello" }
