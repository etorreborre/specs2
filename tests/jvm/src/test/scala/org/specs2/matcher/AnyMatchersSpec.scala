package org.specs2
package matcher

import java.io.*
import execute.*
import language.postfixOps

class AnyMatchersSpec
    extends Specification
    with ResultMatchers
    with AnyMatchers
    with ValueChecks
    with TypecheckMatchers
    with Conversions {
  def is = s2"""

  beTheSameAs checks if a value is eq to another one
  ${aValue must beTheSameAs(aValue)}
  ${"a" must not(beTheSameAs("b"))}

  be is an alias for beTheSameAs
  ${aValue must be(aValue)}
  ${"a" must not(be("b"))}

  be_==~ checks the equality of 2 objects, up to an implicit conversion
  ${1L must be_==~(1)}
  ${2L must not(be_==~(1))}
  ${(2L must be_==~(1)).message must contain("2 != 1")}

  beTrue matches true values
  ${true must beTrue}
  ${(false must beTrue).message must ===("the value is false")}

  beFalse matches false values
  ${false must beFalse}
  ${(true must beFalse).message must ===("the value is true")}

  beEmpty matches empty values
  ${"" must beEmpty}
  ${"x" must not(beEmpty)}
  ${None must beEmpty}
  ${Some(1) must not(beEmpty)}
  ${Seq() must beEmpty}
  ${Seq(1) must not(beEmpty)}
  ${Map() must beEmpty}
  ${Map(1 -> 2) must not(beEmpty)}
  ${Set() must beEmpty}
  ${Set(1) must not(beEmpty)}

  beLike matches objects against a pattern
  ${List(1, 2) must beLike { case List(a, b) => ok }}
  ${List(1, 2) must beLike { case List(a, b) => (a + b) must ===(3) }}
  if the match succeeds but the condition after match fails, a precise failure message can be returned
  ${(List(1, 2) must beLike { case List(a, b) => (a + b) must ===(2) }) returns "3 != 2"}

 forall allows to transform a single matcher to a matcher checking that all elements of a Seq are matching
  ${Seq(2, 3, 4) must contain(be_>=(2)).forall}
  xxx ${forall(Seq((1, 2), (3, 4))) { case (a, b) => a must be_<(b) }}
  ${forallWhen(Seq((2, 1), (3, 4))) { case (a, b) if a > 2 => a must be_<(b) }}

  foreach is like forall but will execute all matchers and collect the results
  ${Seq(2, 3, 4) must contain(be_>=(2)).foreach}
  ${foreach(Seq((1, 2), (3, 4))) { case (a, b) => a must be_<(b) }}
  ${foreachWhen(Seq((2, 1), (3, 4))) { case (a, b) if a > 2 => a must be_<(b) }}
  ${foreach(Seq(2, 3, 4))(i => i must be_>=(2))}
  if all expectations throws are Skipped then the whole result must be skipped $skipForeach

  atLeastOnce allows to transform a single matcher to a matcher checking that one element of a Seq is matching
  ${Seq(2, 3, 4) must contain(be_>(2)).atLeastOnce}
  ${Seq(2, 3, 4).atLeastOnce(i => i must be_>(2))}
  ${Seq(2, 3, 4).atLeastOnce(i => MustExpectations.theValue(i) must be_>(2))}
  ${atLeastOnce(Seq((4, 2), (3, 4))) { case (a, b) => a must be_<(b) }}
  ${atLeastOnceWhen(Seq((2, 1), (3, 4))) { case (a, b) if a > 2 => a must be_<(b) }}
  ${atLeastOnce(Seq(Some(1), None)) { _ must beSome(1) }}
  ${(new org.specs2.mutable.Specification { Seq(1).atLeastOnce(_ must be_<(0)) }: Any) must
      throwA[FailureException]} ${tag("issue #169")}

  beNull matches null values
  ${(null: String) must beNull}
  ${(null: String) must be(null)}
  ${"" must not(beNull)}

  beAsNullAs checks if two values are null at the same time
  ${(null: String) must beAsNullAs(null)}
  ${1 must not(beAsNullAs(null))}
  ${(null: String) must not(beAsNullAs(1))}

  beOneOf checks if a value is amongst others
  ${1 must beOneOf(1, 2, 3)}
  ${4 must not(beOneOf(1, 2, 3))}

  haveClass checks if a value has a given class as its type
  ${(1: java.lang.Integer) must haveClass[java.lang.Integer]}
  ${BigInt(1) must not(haveClass[String])}

  haveSuperclass checks if a value has a given class as one of its ancestors
  ${new BufferedInputStream(null) must haveSuperclass[InputStream]}
  ${BigInt(1) must not(haveSuperclass[String])}

  haveInterface checks if a value has a given interface in the list of its interfaces
  ${AsResult(new java.util.ArrayList() must haveInterface[java.util.List[?]])}
  ${AsResult(BigInt(1) must not(haveInterface[java.util.List[?]]))}

  beAssignableFrom checks if a class is assignable from another
  ${classOf[OutputStream] must beAssignableFrom[FileOutputStream]}

  beAnInstanceOf checks if an object is an instance of a given type
  ${type1 must beAnInstanceOf[Type1]}
  ${type1 must not(beAnInstanceOf[Type2])}
  ${(type1 must beAnInstanceOf[Type2]).message must ===(
      s"'type1: ${type1.getClass.getName}' is not an instance of 'org.specs2.matcher.Type2'"
    )}

Implicits
=========

  the === implicits can be deactivated with the NoCanBeEqual trait $implicits1
  the must implicits can be deactivated with the NoMustExpectations trait $implicits2
"""

  def implicits1 =
    // if this specification compiles and if result is ok, this means that the === implicit could be redefined
    // thanks to the NoCanBeEqual trait
    case class Spec1() extends Specification with NoTypedEqual:
      extension [S, T](t: =>T) def ===(other: S) = other

      val result = (1 === 2) must ===(2)
      def is = result
    Spec1().result

  def implicits2 =
    // if this specification compiles and if result is ok, this means that the must implicit could be redefined
    // thanks to the NoMustExpectations trait
    case class Spec1() extends org.specs2.mutable.Specification with NoMustExpectations:
      extension [T](t: =>T) infix def must(other: Int) = other

      val result = (1 must 2) === 2
      "an example" >> result
    Spec1().result

  val aValue: String = "a value"

  val type1 = new Type1 { override def toString = "type1" }

  def skipForeach = { Result.foreach(Seq(0, 1, 2)) { case a => a must be_<(0).orSkip("todo") } } must beLike {
    case s: Skipped => ok
  }
}

trait Type1
trait Type2

trait Conversions:
  given Conversion[Int, Long] with
    def apply(n: Int): Long = n.toLong
