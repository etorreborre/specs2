package org.specs2
package specification

import execute._

/**
 * This trait can be used to standardize names for groups of examples in an acceptance specification.
 *
 * class MySpecification extends Specification with Examples { def is = s2"""
 *   first example in first group                                       ${g1().e1}
 *   second example in first group                                      ${g1().e2}
 *
 *   first example in second group                                      ${g2().e1}
 *   second example in second group                                     ${g2().e2}
 *                                                                      """
 * }
 *
 * trait Examples extends Groups with Matchers {
 *   "first group of examples" - new g1 {
 *     e1 := ok
 *     e2 ;= ok
 *   }
 *   "second group of examples" - new g2 {
 *     e1 := ok
 *     e2 := ok
 *   }
 * }
 *
 * It is important to notice that the examples must be called with `g1().e1` so as to create a new `g1` trait instance
 * with new local variables for the example `e1`. If this is not required, the `Grouped` trait can be used instead
 *
 * If you don't want to manage groups and examples numbers it is also possible to write the following (note the `script.Specification`):
 *
 * class MySpecification extends script.Specification with Examples { def is = s2"""
 *   first example in first group
 *   second example in first group
 *
 *   first example in second group
 *   second example in second group
 *                                                   """
 * }
 *
 * trait Examples extends Groups with Matchers {
 *   "first group of examples" - new group {
 *     eg := ok
 *     eg := ok
 *   }
 *   "second group of examples" - new group {
 *     eg := ok
 *     eg := ok
 *   }
 * }

 */
trait Groups extends GroupsLike { outer =>
  def group(i: Int): ExamplesGroup = {
    if (autoNumberedGroups.nonEmpty) autoNumberedGroups.applyOrElse(i, (n: Int) => g1)()
    else                             numberedExampleGroups.applyOrElse(i, (n: Int) => g1)()
  }

  trait g1  extends ExamplesGroup
  trait g2  extends ExamplesGroup
  trait g3  extends ExamplesGroup
  trait g4  extends ExamplesGroup
  trait g5  extends ExamplesGroup
  trait g6  extends ExamplesGroup
  trait g7  extends ExamplesGroup
  trait g8  extends ExamplesGroup
  trait g9  extends ExamplesGroup
  trait g10 extends ExamplesGroup
  trait g11 extends ExamplesGroup
  trait g12 extends ExamplesGroup
  trait g13 extends ExamplesGroup
  trait g14 extends ExamplesGroup
  trait g15 extends ExamplesGroup
  trait g16 extends ExamplesGroup
  trait g17 extends ExamplesGroup
  trait g18 extends ExamplesGroup
  trait g19 extends ExamplesGroup
  trait g20 extends ExamplesGroup
  trait g21 extends ExamplesGroup
  trait g22 extends ExamplesGroup

  var (g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22) =
    (() => (new g1  {}.copy("g1" )): ExamplesGroup,
     () => (new g2  {}.copy("g2" )): ExamplesGroup,
     () => (new g3  {}.copy("g3" )): ExamplesGroup,
     () => (new g4  {}.copy("g4" )): ExamplesGroup,
     () => (new g5  {}.copy("g5" )): ExamplesGroup,
     () => (new g6  {}.copy("g6" )): ExamplesGroup,
     () => (new g7  {}.copy("g7" )): ExamplesGroup,
     () => (new g8  {}.copy("g8" )): ExamplesGroup,
     () => (new g9  {}.copy("g9" )): ExamplesGroup,
     () => (new g10 {}.copy("g10")): ExamplesGroup,
     () => (new g11 {}.copy("g11")): ExamplesGroup,
     () => (new g12 {}.copy("g12")): ExamplesGroup,
     () => (new g13 {}.copy("g13")): ExamplesGroup,
     () => (new g14 {}.copy("g14")): ExamplesGroup,
     () => (new g15 {}.copy("g15")): ExamplesGroup,
     () => (new g16 {}.copy("g16")): ExamplesGroup,
     () => (new g17 {}.copy("g17")): ExamplesGroup,
     () => (new g18 {}.copy("g18")): ExamplesGroup,
     () => (new g19 {}.copy("g19")): ExamplesGroup,
     () => (new g20 {}.copy("g20")): ExamplesGroup,
     () => (new g21 {}.copy("g21")): ExamplesGroup,
     () => (new g22 {}.copy("g22")): ExamplesGroup)

  implicit def namedGroup(s: String): NamedGroup = new NamedGroup(s)

  class NamedGroup(s: String) {
    def -(g: =>ExamplesGroup) = storeGroup(g)
  }

  private def storeGroup(g: =>ExamplesGroup): Unit = {
    if      (g.isInstanceOf[g1] )                { storeGroup(1 , g) }
    else if (g.isInstanceOf[g2] )                { storeGroup(2 , g) }
    else if (g.isInstanceOf[g3] )                { storeGroup(3 , g) }
    else if (g.isInstanceOf[g4] )                { storeGroup(4 , g) }
    else if (g.isInstanceOf[g5 ])                { storeGroup(5 , g) }
    else if (g.isInstanceOf[g6 ])                { storeGroup(6 , g) }
    else if (g.isInstanceOf[g7 ])                { storeGroup(7 , g) }
    else if (g.isInstanceOf[g8 ])                { storeGroup(8 , g) }
    else if (g.isInstanceOf[g9 ])                { storeGroup(9 , g) }
    else if (g.isInstanceOf[g10])                { storeGroup(10, g) }
    else if (g.isInstanceOf[g11])                { storeGroup(11, g) }
    else if (g.isInstanceOf[g12])                { storeGroup(12, g) }
    else if (g.isInstanceOf[g13])                { storeGroup(13, g) }
    else if (g.isInstanceOf[g14])                { storeGroup(14, g) }
    else if (g.isInstanceOf[g15])                { storeGroup(15, g) }
    else if (g.isInstanceOf[g16])                { storeGroup(16, g) }
    else if (g.isInstanceOf[g17])                { storeGroup(17, g) }
    else if (g.isInstanceOf[g18])                { storeGroup(18, g) }
    else if (g.isInstanceOf[g19])                { storeGroup(19, g) }
    else if (g.isInstanceOf[g20])                { storeGroup(20, g) }
    else if (g.isInstanceOf[g21])                { storeGroup(21, g) }
    else if (g.isInstanceOf[AutoNumberedGroup])  { autoNumberedGroups = autoNumberedGroups :+ (() => g) }
  }

  private def storeGroup(i: Int, g: =>ExamplesGroup): Unit = {
    if      (i == 1 ) { g1  = () => g; g1  }
    else if (i == 2 ) { g2  = () => g; g2  }
    else if (i == 3 ) { g3  = () => g; g3  }
    else if (i == 4 ) { g4  = () => g; g4  }
    else if (i == 5 ) { g5  = () => g; g5  }
    else if (i == 6 ) { g6  = () => g; g6  }
    else if (i == 7 ) { g7  = () => g; g7  }
    else if (i == 8 ) { g8  = () => g; g8  }
    else if (i == 9 ) { g9  = () => g; g9  }
    else if (i == 10) { g10 = () => g; g10 }
    else if (i == 11) { g11 = () => g; g11 }
    else if (i == 12) { g12 = () => g; g12 }
    else if (i == 13) { g13 = () => g; g13 }
    else if (i == 14) { g14 = () => g; g14 }
    else if (i == 15) { g15 = () => g; g15 }
    else if (i == 16) { g16 = () => g; g16 }
    else if (i == 17) { g17 = () => g; g17 }
    else if (i == 18) { g18 = () => g; g18 }
    else if (i == 19) { g19 = () => g; g19 }
    else if (i == 20) { g20 = () => g; g20 }
    else if (i == 21) { g21 = () => g; g21 }
    else if (i == 22) { g22 = () => g; g22 }
  }

  private lazy val numberedExampleGroups = Seq(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22)
  private var autoNumberedGroups: Seq[() => ExamplesGroup] = Seq()
}

/**
 * This trait can be used to standardize names for groups of examples in an acceptance specification.
 *
 * class MySpecification extends Specification with Examples { def is = s2"""
 *   first example in first group                                       ${g1.e1}
 *   second example in first group                                      ${g1.e2}
 *
 *   first example in second group                                      ${g2.e1}
 *   second example in second group                                     ${g2.e2}
 *                                                                      """
 * }
 *
 * trait Examples extends Grouped with Matchers {
 *   "first group of examples" - new g1 {
 *     e1 := ok
 *     e2 := ok
 *   }
 *   "second group of examples" - new g2 {
 *     e1 := ok
 *     e2 := ok
 *   }
 * }
 *
 * If you don't want to manage groups and examples numbers it is also possible to write the following (note the `script.Specification`):
 *
 * class MySpecification extends script.Specification with Examples { def is = s2"""
 *   first example in first group
 *   second example in first group
 *
 *   first example in second group
 *   second example in second group
 *                                                   """
 * }
 *
 * trait Examples extends Grouped with Matchers {
 *   "first group of examples" - new group {
 *     eg := ok
 *     eg := ok
 *   }
 *   "second group of examples" - new group {
 *     eg := ok
 *     eg := ok
 *   }
 * }
 */
trait Grouped extends GroupsLike { outer =>
  def group(i: Int): ExamplesGroup = {
    if (autoNumberedGroups.nonEmpty) autoNumberedGroups.applyOrElse(i, (n: Int) => g1)
    else                             numberedExampleGroups.applyOrElse(i, (n: Int) => g1)
  }

  trait g1  extends ExamplesGroup { outer.g1 = this }
  trait g2  extends ExamplesGroup { outer.g2 = this }
  trait g3  extends ExamplesGroup { outer.g3 = this }
  trait g4  extends ExamplesGroup { outer.g4 = this }
  trait g5  extends ExamplesGroup { outer.g5  = this }
  trait g6  extends ExamplesGroup { outer.g6  = this }
  trait g7  extends ExamplesGroup { outer.g7  = this }
  trait g8  extends ExamplesGroup { outer.g8  = this }
  trait g9  extends ExamplesGroup { outer.g9  = this }
  trait g10 extends ExamplesGroup { outer.g10 = this }
  trait g11 extends ExamplesGroup { outer.g11 = this }
  trait g12 extends ExamplesGroup { outer.g12 = this }
  trait g13 extends ExamplesGroup { outer.g13 = this }
  trait g14 extends ExamplesGroup { outer.g14 = this }
  trait g15 extends ExamplesGroup { outer.g15 = this }
  trait g16 extends ExamplesGroup { outer.g16 = this }
  trait g17 extends ExamplesGroup { outer.g17 = this }
  trait g18 extends ExamplesGroup { outer.g18 = this }
  trait g19 extends ExamplesGroup { outer.g19 = this }
  trait g20 extends ExamplesGroup { outer.g20 = this }
  trait g21 extends ExamplesGroup { outer.g21 = this }
  trait g22 extends ExamplesGroup { outer.g22 = this }

  var (g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22) =
    ((new g1  {}).copy("g1" ): ExamplesGroup,
     (new g2  {}).copy("g2" ): ExamplesGroup,
     (new g3  {}).copy("g3" ): ExamplesGroup,
     (new g4  {}).copy("g4" ): ExamplesGroup,
     (new g5  {}).copy("g5" ): ExamplesGroup,
     (new g6  {}).copy("g6" ): ExamplesGroup,
     (new g7  {}).copy("g7" ): ExamplesGroup,
     (new g8  {}).copy("g8" ): ExamplesGroup,
     (new g9  {}).copy("g9" ): ExamplesGroup,
     (new g10 {}).copy("g10"): ExamplesGroup,
     (new g11 {}).copy("g11"): ExamplesGroup,
     (new g12 {}).copy("g12"): ExamplesGroup,
     (new g13 {}).copy("g13"): ExamplesGroup,
     (new g14 {}).copy("g14"): ExamplesGroup,
     (new g15 {}).copy("g15"): ExamplesGroup,
     (new g16 {}).copy("g16"): ExamplesGroup,
     (new g17 {}).copy("g17"): ExamplesGroup,
     (new g18 {}).copy("g18"): ExamplesGroup,
     (new g19 {}).copy("g19"): ExamplesGroup,
     (new g20 {}).copy("g20"): ExamplesGroup,
     (new g21 {}).copy("g21"): ExamplesGroup,
     (new g22 {}).copy("g22"): ExamplesGroup)



  implicit def namedGroup(s: String): NamedGroup = new NamedGroup(s)
  class NamedGroup(s: String) {

    def -(g: =>ExamplesGroup) {
      val examplesGroup = g
      examplesGroup.nameIs(s)
      if (examplesGroup.isInstanceOf[AutoNumberedGroup]) {
        autoNumberedGroups = autoNumberedGroups :+ g
      }
    }
  }

  private lazy val numberedExampleGroups = Seq(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22)
  private var autoNumberedGroups: Seq[ExamplesGroup] = Seq()
}

trait GroupsLike {
  trait AutoNumberedGroup extends ExamplesGroup {
    private var autoNumberedExamples: Seq[Function0Result] = Seq()

    override def example(i: Int) =
      if (autoNumberedExamples.nonEmpty) autoNumberedExamples.applyOrElse(i, (index:Int) => Function0Result.anyToAnyResult(new execute.Pending(s" - PENDING ")))
      else                               numberedExamples(i)

    def eg = this

    def :=[R: AsResult](r: =>R) {
      autoNumberedExamples = autoNumberedExamples :+ (Function0Result.anyToAnyResult(r))
    }
  }
  trait group extends AutoNumberedGroup

  def group(i: Int): ExamplesGroup
}

case class ExamplesGroup(private var name: String = "") {
  def groupName = name

  def nameIs(n: String) = { name = s"'$n'"; this }

  val e1:  Function0Result = new execute.Pending(s" - PENDING")
  val e2:  Function0Result = new execute.Pending(s" - PENDING")
  val e3:  Function0Result = new execute.Pending(s" - PENDING")
  val e4:  Function0Result = new execute.Pending(s" - PENDING")
  val e5:  Function0Result = new execute.Pending(s" - PENDING")
  val e6:  Function0Result = new execute.Pending(s" - PENDING")
  val e7:  Function0Result = new execute.Pending(s" - PENDING")
  val e8:  Function0Result = new execute.Pending(s" - PENDING")
  val e9:  Function0Result = new execute.Pending(s" - PENDING")
  val e10: Function0Result = new execute.Pending(s" - PENDING")
  val e11: Function0Result = new execute.Pending(s" - PENDING")
  val e12: Function0Result = new execute.Pending(s" - PENDING")
  val e13: Function0Result = new execute.Pending(s" - PENDING")
  val e14: Function0Result = new execute.Pending(s" - PENDING")
  val e15: Function0Result = new execute.Pending(s" - PENDING")
  val e16: Function0Result = new execute.Pending(s" - PENDING")
  val e17: Function0Result = new execute.Pending(s" - PENDING")
  val e18: Function0Result = new execute.Pending(s" - PENDING")
  val e19: Function0Result = new execute.Pending(s" - PENDING")
  val e20: Function0Result = new execute.Pending(s" - PENDING")
  val e21: Function0Result = new execute.Pending(s" - PENDING")
  val e22: Function0Result = new execute.Pending(s" - PENDING")

  protected lazy val numberedExamples = Seq(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18,e19,e20,e21,e22)

  def example(i: Int) = numberedExamples(i)
}
