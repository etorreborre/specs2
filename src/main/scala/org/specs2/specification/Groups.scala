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
trait Groups { outer =>

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
    (() => new g1  {}: Group,
     () => new g2  {}: Group,
     () => new g3  {}: Group,
     () => new g4  {}: Group,
     () => new g5  {}: Group,
     () => new g6  {}: Group,
     () => new g7  {}: Group,
     () => new g8  {}: Group,
     () => new g9  {}: Group,
     () => new g10 {}: Group,
     () => new g11 {}: Group,
     () => new g12 {}: Group,
     () => new g13 {}: Group,
     () => new g14 {}: Group,
     () => new g15 {}: Group,
     () => new g16 {}: Group,
     () => new g17 {}: Group,
     () => new g18 {}: Group,
     () => new g19 {}: Group,
     () => new g20 {}: Group,
     () => new g21 {}: Group,
     () => new g22 {}: Group)

  implicit def namedGroup(s: String): NamedGroup = new NamedGroup(s)
  class NamedGroup(s: String) {
    def -(g: =>Group) =
      if      (g.isInstanceOf[g1])  { g1 = () => g }
      else if (g.isInstanceOf[g2])  { g2 = () => g }
      else if (g.isInstanceOf[g3])  { g3 = () => g }
      else if (g.isInstanceOf[g4])  { g4 = () => g }
      else if (g.isInstanceOf[g5 ]) { g5  = () => g }
      else if (g.isInstanceOf[g6 ]) { g6  = () => g }
      else if (g.isInstanceOf[g7 ]) { g7  = () => g }
      else if (g.isInstanceOf[g8 ]) { g8  = () => g }
      else if (g.isInstanceOf[g9 ]) { g9  = () => g }
      else if (g.isInstanceOf[g10]) { g10 = () => g }
      else if (g.isInstanceOf[g11]) { g11 = () => g }
      else if (g.isInstanceOf[g12]) { g12 = () => g }
      else if (g.isInstanceOf[g13]) { g13 = () => g }
      else if (g.isInstanceOf[g14]) { g14 = () => g }
      else if (g.isInstanceOf[g15]) { g15 = () => g }
      else if (g.isInstanceOf[g16]) { g16 = () => g }
      else if (g.isInstanceOf[g17]) { g17 = () => g }
      else if (g.isInstanceOf[g18]) { g18 = () => g }
      else if (g.isInstanceOf[g19]) { g19 = () => g }
      else if (g.isInstanceOf[g20]) { g20 = () => g }
      else if (g.isInstanceOf[g21]) { g21 = () => g }
      else if (g.isInstanceOf[g22]) { g22 = () => g }
  }
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
trait Grouped { outer =>

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
    (new g1  {}: Group,
     new g2  {}: Group,
     new g3  {}: Group,
     new g4  {}: Group,
     new g5  {}: Group,
     new g6  {}: Group,
     new g7  {}: Group,
     new g8  {}: Group,
     new g9  {}: Group,
     new g10 {}: Group,
     new g11 {}: Group,
     new g12 {}: Group,
     new g13 {}: Group,
     new g14 {}: Group,
     new g15 {}: Group,
     new g16 {}: Group,
     new g17 {}: Group,
     new g18 {}: Group,
     new g19 {}: Group,
     new g20 {}: Group,
     new g21 {}: Group,
     new g22 {}: Group)

  implicit def namedGroup(s: String): NamedGroup = new NamedGroup(s)
  class NamedGroup(s: String) {
    def -(g: Group) = g
  }
}

case class Group(private val name: String = "") extends BeforeAfterAround {
  private lazy val pending = StandardResults.pending

  val e1:  Function0Result = pending
  val e2:  Function0Result = pending
  val e3:  Function0Result = pending
  val e4:  Function0Result = pending
  val e5:  Function0Result = pending
  val e6:  Function0Result = pending
  val e7:  Function0Result = pending
  val e8:  Function0Result = pending
  val e9:  Function0Result = pending
  val e10: Function0Result = pending
  val e11: Function0Result = pending
  val e12: Function0Result = pending
  val e13: Function0Result = pending
  val e14: Function0Result = pending
  val e15: Function0Result = pending
  val e16: Function0Result = pending
  val e17: Function0Result = pending
  val e18: Function0Result = pending
  val e19: Function0Result = pending
  val e20: Function0Result = pending
  val e21: Function0Result = pending
  val e22: Function0Result = pending

  def before {}
  def after {}
  def around[T : AsResult](a: =>T): Result = AsResult(a)

}
