package org.specs2
package matcher

import java.io._
import execute._
import specification._

class AnyMatchersSpec extends script.Specification with Groups with ResultMatchers { def is = s2"""

  be_== checks the equality of 2 objects
  ${ "a" must_== "a" }
  ${ "a" must not be_==(null) }
  ${ (null: String) must not be_==("a") }
  ${ "a" must_!= "b" }
  ${ "a" should_== "a" }
  ${ "a" should_!= "b" }
  ${ "a" must be_==("a") }
  ${ "a" must not be_==("b") }
  ${ "a" must be_!=("b") }
  ${ "a" must not be_!=("a") }
  ${ "a" === "a" }
  ${ "a" !== "b" }
  ${ "a" must be_===("a") }
  // doesn't compile
  // { "a" ==== 1 }
  ${ "a" must not be_===("b") }
  ${ "a" must be_!==("b") }
  ${ "a" must not be_!==("a") }
  ${ Array(1, 2) must be_==(Array(1, 2)) }
  ${ Array(1, 3) must not be_==(Array(1, 2)) }
  ${ Array(1, 2) must be_===(Array(1, 2)) }
  ${ Array(1, 3) must not be_===(Array(1, 2)) }
  ${ (Array(1, 3) must not be_===(Array(1, 2))).message === "Array(1, 3) is not equal to Array(1, 2)" }
  ${ Array(Array(1, 2)) must be_==(Array(Array(1, 2))) }
  ${ Array(1, 2) must be_==(Seq(1, 2)) }
  ${ Seq(1, 2) must be_==(Array(1, 2)) }
  ${ (1 must_== 2).toResult must beLike { case Failure(_,_,_,FailureDetails(e, a)) => e must_== "2" } }
  the actual value must be evaluated before the expected one
  ${ var result = "";
     {{ result = result + "a" }; 1} must_== {{ result = result + "b" }; 1}
     result must_== "ab"
   }

  beTheSameAs checks if a value is eq to another one
  ${ aValue must beTheSameAs(aValue) }
  ${ "a" must not beTheSameAs("b") }

  be is an alias for beTheSameAs
  ${ aValue must be(aValue) }
  ${ "a" must not be("b") }

  be_==~ checks the equality of 2 objects, up to an implicit conversion
  ${ 1L must be_==~(1) }
  ${ 2L must not be_==~(1) }
  ${ (2L must be_==~(1)).message must contain("'2' is not equal to '1'") }

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
  ${ Seq(2, 3, 4) must be_>=(2).forall }
  ${ forall(Seq((1, 2), (3, 4))) { case (a, b) => a must be_<(b) } }
  ${ forallWhen(Seq((2, 1), (3, 4))) { case (a, b) if a > 2 => a must be_<(b) } }
  ${ (Seq(2, 3, 4) must contain(_:Int)).forall(Seq(2, 4)) }

  foreach is like forall but will execute all matchers and collect the results
  ${ Seq(2, 3, 4) must be_>=(2).foreach }
  ${ foreach(Seq((1, 2), (3, 4))) { case (a, b) => a must be_<(b) } }
  ${ foreachWhen(Seq((2, 1), (3, 4))) { case (a, b) if a > 2 => a must be_<(b) } }
  ${ ((_:Int) must be_>=(2)).foreach(Seq(2, 3, 4)) }
  if all expectations throws are Skipped then the whole result must be skipped $skipForeach

  atLeastOnce allows to transform a single matcher to a matcher checking that one element of a Seq is matching
  ${ Seq(2, 3, 4) must be_>(2).atLeastOnce }
  ${ ((_:Int) must be_>(2)).atLeastOnce(Seq(2, 3, 4)) }
  ${ ((i:Int) => MustExpectations.theValue(i) must be_>(2)).atLeastOnce(Seq(2, 3, 4)) }
  ${ atLeastOnce(Seq((4, 2), (3, 4))) { case (a, b) => a must be_<(b) } }
  ${ atLeastOnceWhen(Seq((2, 1), (3, 4))) { case (a, b) if a > 2 => a must be_<(b) } }
  ${ atLeastOnce(Seq(Some(1), None)) { _ must beSome(1) } }
  ${ (new MatchersImplicits with ThrownExpectations).atLeastOnce(Seq(1))(_ must be_<(0)) must throwA[FailureException] } ${tag("issue #169")}

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
  ${ 1 must haveClass[java.lang.Integer] }
  ${ 1 must not have klass[String] }

  haveSuperclass checks if a value has a given class as one of its ancestors
  ${ new BufferedInputStream(null) must haveSuperclass[InputStream] }
  ${ 1 must not have superClass[String] }

  haveInterface checks if a value has a given interface in the list of its interfaces
  ${ AsResult(new java.util.ArrayList() must haveInterface[java.util.List[_]]) }
  ${ 1 must not have interface[java.util.List[_]] : Result }

  beAssignableFrom checks if a class is assignable from another
  ${ classOf[OutputStream] must beAssignableFrom[FileOutputStream] }

  beAnInstanceOf checks if an object is an instance of a given type
  ${ type1 must beAnInstanceOf[Type1] }
  ${ type1 must not be anInstanceOf[Type2] }
  ${ (type1 must beAnInstanceOf[Type2]).message must_== "'type1' is not an instance of 'org.specs2.matcher.Type2'" }

Implicits
=========

  + the === implicits can be deactivated with the NoCanBeEqual trait
  + the must implicits can be deactivated with the NoMustExpectations trait

Robustness
==========

  the be_== matcher must be robust in face of
   + a null object
   + a non-traversable collection

Details
=======

  the be_== matcher must warn when comparing 2 objects with the same toString representation but not the same type"
   + with List[Int] and List[String]
   + with 'hello': String and 'hello': Hello
   + with List("1, 2") and List("1", "2")
   + with Map(1 -> "2") and Map(1 -> 2)
                                                                                                                        """
  "be like" - new group {
    eg := { (List(1, 2) must beLike { case List(a, b) => (a + b) must_== 2 }) returns "'3' is not equal to '2'" }
  }

  "must implicits" - new group {

    eg := {
      // if this specification compiles and if result is ok, this means that the === implicit could be redefined
      // thanks to the NoCanBeEqual trait
      val spec = new Specification with NoCanBeEqual {
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

  "robustness" - new group {
    eg := ((null: String) must_== "1") must not(throwAn[Exception])
    eg := {
      def newTraversable = new TraversableWithNoDefinedForeach[Int] {}
      val (t1, t2) = (newTraversable, newTraversable)
      (t1 must_== t2) must not(throwAn[Exception])
    }
  }

  "details" - new group {
    eg := {
      (List(1, 2) must_== List("1", "2")) must beFailing(
        "\\Q'List('1', '2'): scala.collection.immutable.$colon$colon[java.lang.Integer]'\n is not equal to \n'List('1', '2'): scala.collection.immutable.$colon$colon[java.lang.String]'\\E")
    }
    eg := {
      ("hello" must_== Hello()) must beFailing(
        "\\Q'hello: java.lang.String' is not equal to 'hello: org.specs2.matcher.Hello'\\E")
    }
    eg := {
      (List("1, 2") must_== List("1", "2")) must beFailing(
        "\\Q'List('1, 2'): scala.collection.immutable.$colon$colon[java.lang.String]'\n is not equal to \n'List('1', '2'): scala.collection.immutable.$colon$colon[java.lang.String]'\\E")
    }
    eg := {
      (Map(1 -> "2") must_== Map(1 -> 2)) must beFailing(
        "\\Q'Map(1: java.lang.Integer -> 2: java.lang.String): scala.collection.immutable.Map$Map1'\n is not equal to \n'Map(1: java.lang.Integer -> 2: java.lang.Integer): scala.collection.immutable.Map$Map1'\\E")
    }
  }

  val aValue: String = "a value"

  val type1 = new Type1 { override def toString = "type1" }

  def skipForeach =
    { foreach(Seq(0, 1, 2)) { case a => a must be_<(0).orSkip("todo") } } must beLike { case MatchSkip(_,_) => ok }
}

trait TraversableWithNoDefinedForeach[T] extends Traversable[T] {
  def foreach[U](f: T => U): Unit = sys.error("foreach is not defined on this traversable but toString is")
}

trait Type1
trait Type2

case class Hello() { override def toString = "hello" }
