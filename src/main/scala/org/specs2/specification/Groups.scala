package org.specs2
package specification

import execute._

/**
 * This trait can be used to standardize names for groups of examples in an acceptance specification.
 *
 * class MySpecification extends Examples { def is =
 *   "first example in first group"   ! g1().e1 ^
 *   "second example in first group"  ! g1().e2 ^
 *                                              p^
 *   "first example in second group"  ! g2().e1 ^
 *   "second example in second group" ! g2().e2
 * }
 *
 * trait Examples extends Groups with Matchers {
 *   "first group of examples" - new g1 {
 *     e1 - ok
 *     e2 - ok
 *   }
 *   "second group of examples" - new g2 {
 *     e1 - ok
 *     e2 - ok
 *   }
 * }
 *
 * It is important to notice that the examples must be called with `g1().e1` so as to create a new `g1` trait instance
 * with new local variables for the example `e1`. If this is not required, the `Grouped` trait can be used instead
 *
 */
trait Groups extends GroupsLike { outer =>
  def group(i: Int): Group = allExampleGroups(i)()

  trait g1  extends Group
  trait g2  extends Group
  trait g3  extends Group
  trait g4  extends Group
  trait g5  extends Group
  trait g6  extends Group
  trait g7  extends Group
  trait g8  extends Group
  trait g9  extends Group
  trait g10 extends Group
  trait g11 extends Group
  trait g12 extends Group
  trait g13 extends Group
  trait g14 extends Group
  trait g15 extends Group
  trait g16 extends Group
  trait g17 extends Group
  trait g18 extends Group
  trait g19 extends Group
  trait g20 extends Group
  trait g21 extends Group
  trait g22 extends Group

  var (g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22) =
    (() => (new g1  {}.copy("g1" )): Group,
     () => (new g2  {}.copy("g2" )): Group,
     () => (new g3  {}.copy("g3" )): Group,
     () => (new g4  {}.copy("g4" )): Group,
     () => (new g5  {}.copy("g5" )): Group,
     () => (new g6  {}.copy("g6" )): Group,
     () => (new g7  {}.copy("g7" )): Group,
     () => (new g8  {}.copy("g8" )): Group,
     () => (new g9  {}.copy("g9" )): Group,
     () => (new g10 {}.copy("g10")): Group,
     () => (new g11 {}.copy("g11")): Group,
     () => (new g12 {}.copy("g12")): Group,
     () => (new g13 {}.copy("g13")): Group,
     () => (new g14 {}.copy("g14")): Group,
     () => (new g15 {}.copy("g15")): Group,
     () => (new g16 {}.copy("g16")): Group,
     () => (new g17 {}.copy("g17")): Group,
     () => (new g18 {}.copy("g18")): Group,
     () => (new g19 {}.copy("g19")): Group,
     () => (new g20 {}.copy("g20")): Group,
     () => (new g21 {}.copy("g21")): Group,
     () => (new g22 {}.copy("g22")): Group)

  implicit def namedGroup(s: String): NamedGroup = new NamedGroup(s)
  class NamedGroup(s: String) {
    def -(g: =>Group) = {
      if      (g.isInstanceOf[g1])  { g1  = () => g ; g1  }
      else if (g.isInstanceOf[g2])  { g2  = () => g ; g2  }
      else if (g.isInstanceOf[g3])  { g3  = () => g ; g3  }
      else if (g.isInstanceOf[g4])  { g4  = () => g ; g4  }
      else if (g.isInstanceOf[g5 ]) { g5  = () => g ; g5  }
      else if (g.isInstanceOf[g6 ]) { g6  = () => g ; g6  }
      else if (g.isInstanceOf[g7 ]) { g7  = () => g ; g7  }
      else if (g.isInstanceOf[g8 ]) { g8  = () => g ; g8  }
      else if (g.isInstanceOf[g9 ]) { g9  = () => g ; g9  }
      else if (g.isInstanceOf[g10]) { g10 = () => g ; g10 }
      else if (g.isInstanceOf[g11]) { g11 = () => g ; g11 }
      else if (g.isInstanceOf[g12]) { g12 = () => g ; g12 }
      else if (g.isInstanceOf[g13]) { g13 = () => g ; g13 }
      else if (g.isInstanceOf[g14]) { g14 = () => g ; g14 }
      else if (g.isInstanceOf[g15]) { g15 = () => g ; g15 }
      else if (g.isInstanceOf[g16]) { g16 = () => g ; g16 }
      else if (g.isInstanceOf[g17]) { g17 = () => g ; g17 }
      else if (g.isInstanceOf[g18]) { g18 = () => g ; g18 }
      else if (g.isInstanceOf[g19]) { g19 = () => g ; g19 }
      else if (g.isInstanceOf[g20]) { g20 = () => g ; g20 }
      else if (g.isInstanceOf[g21]) { g21 = () => g ; g21 }
      else if (g.isInstanceOf[g22]) { g22 = () => g ; g22 }

    }
  }

  private lazy val allExampleGroups = Seq(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22)
}

/**
 * This trait can be used to standardize names for groups of examples in an acceptance specification.
 *
 * class MySpecification extends Examples { def is =
 *   "first example in first group"   ! g1.e1 ^
 *   "second example in first group"  ! g1.e2 ^
 *                                            p^
 *   "first example in second group"  ! g2.e1 ^
 *   "second example in second group" ! g2.e2
 * }
 *
 * trait Examples extends Grouped with Matchers {
 *   "first group of examples" - new g1 {
 *     e1 - ok
 *     e2 - ok
 *   }
 *   "second group of examples" - new g2 {
 *     e1 - ok
 *     e2 - ok
 *   }
 * }
 *
 * It is important to notice that the examples must be called with `g1().e1` so as to create a new `g1` trait instance
 * with new local variables for the example `e1`. If this is not required, the `Grouped` trait can be used instead
 *
 */
trait Grouped extends GroupsLike { outer =>
  def group(i: Int): Group = allExampleGroups(i)

  trait g1  extends Group { outer.g1 = this }
  trait g2  extends Group { outer.g2 = this }
  trait g3  extends Group { outer.g3 = this }
  trait g4  extends Group { outer.g4 = this }
  trait g5  extends Group { outer.g5  = this }
  trait g6  extends Group { outer.g6  = this }
  trait g7  extends Group { outer.g7  = this }
  trait g8  extends Group { outer.g8  = this }
  trait g9  extends Group { outer.g9  = this }
  trait g10 extends Group { outer.g10 = this }
  trait g11 extends Group { outer.g11 = this }
  trait g12 extends Group { outer.g12 = this }
  trait g13 extends Group { outer.g13 = this }
  trait g14 extends Group { outer.g14 = this }
  trait g15 extends Group { outer.g15 = this }
  trait g16 extends Group { outer.g16 = this }
  trait g17 extends Group { outer.g17 = this }
  trait g18 extends Group { outer.g18 = this }
  trait g19 extends Group { outer.g19 = this }
  trait g20 extends Group { outer.g20 = this }
  trait g21 extends Group { outer.g21 = this }
  trait g22 extends Group { outer.g22 = this }

  var (g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22) =
    ((new g1  {}).copy("g1" ): Group,
     (new g2  {}).copy("g2" ): Group,
     (new g3  {}).copy("g3" ): Group,
     (new g4  {}).copy("g4" ): Group,
     (new g5  {}).copy("g5" ): Group,
     (new g6  {}).copy("g6" ): Group,
     (new g7  {}).copy("g7" ): Group,
     (new g8  {}).copy("g8" ): Group,
     (new g9  {}).copy("g9" ): Group,
     (new g10 {}).copy("g10"): Group,
     (new g11 {}).copy("g11"): Group,
     (new g12 {}).copy("g12"): Group,
     (new g13 {}).copy("g13"): Group,
     (new g14 {}).copy("g14"): Group,
     (new g15 {}).copy("g15"): Group,
     (new g16 {}).copy("g16"): Group,
     (new g17 {}).copy("g17"): Group,
     (new g18 {}).copy("g18"): Group,
     (new g19 {}).copy("g19"): Group,
     (new g20 {}).copy("g20"): Group,
     (new g21 {}).copy("g21"): Group,
     (new g22 {}).copy("g22"): Group)

  implicit def namedGroup(s: String): NamedGroup = new NamedGroup(s)
  class NamedGroup(s: String) {
    def -(g: Group) = g.nameIs(s)
  }

  lazy val allExampleGroups = Seq(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22)
}

trait GroupsLike {
  def group(i: Int): Group
}

case class Group(private var name: String = "") extends BeforeAfterAround {
  def groupName = name

  private lazy val pending = StandardResults.pending
  def nameIs(n: String) = { name = s"'$n'"; this }

  val e1:  Function0Result = new execute.Pending(s" - $name.e1  PENDING ")
  val e2:  Function0Result = new execute.Pending(s" - $name.e2  PENDING ")
  val e3:  Function0Result = new execute.Pending(s" - $name.e3  PENDING ")
  val e4:  Function0Result = new execute.Pending(s" - $name.e4  PENDING ")
  val e5:  Function0Result = new execute.Pending(s" - $name.e5  PENDING ")
  val e6:  Function0Result = new execute.Pending(s" - $name.e6  PENDING ")
  val e7:  Function0Result = new execute.Pending(s" - $name.e7  PENDING ")
  val e8:  Function0Result = new execute.Pending(s" - $name.e8  PENDING ")
  val e9:  Function0Result = new execute.Pending(s" - $name.e9  PENDING ")
  val e10: Function0Result = new execute.Pending(s" - $name.e10 PENDING ")
  val e11: Function0Result = new execute.Pending(s" - $name.e11 PENDING ")
  val e12: Function0Result = new execute.Pending(s" - $name.e12 PENDING ")
  val e13: Function0Result = new execute.Pending(s" - $name.e13 PENDING ")
  val e14: Function0Result = new execute.Pending(s" - $name.e14 PENDING ")
  val e15: Function0Result = new execute.Pending(s" - $name.e15 PENDING ")
  val e16: Function0Result = new execute.Pending(s" - $name.e16 PENDING ")
  val e17: Function0Result = new execute.Pending(s" - $name.e17 PENDING ")
  val e18: Function0Result = new execute.Pending(s" - $name.e18 PENDING ")
  val e19: Function0Result = new execute.Pending(s" - $name.e19 PENDING ")
  val e20: Function0Result = new execute.Pending(s" - $name.e20 PENDING ")
  val e21: Function0Result = new execute.Pending(s" - $name.e21 PENDING ")
  val e22: Function0Result = new execute.Pending(s" - $name.e22 PENDING ")

  private lazy val allExamples = Seq(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18,e19,e20,e21,e22)

  def example(i: Int) = allExamples(i)
  def before {}
  def after {}
  def around[T : AsResult](a: =>T): Result = AsResult(a)

}
