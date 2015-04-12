package org.specs2
package specification

import java.util.concurrent.ExecutorService

import execute._
import control.ImplicitParameters._
import specification.core._
import specification.create._

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
trait Groups extends GroupsLike { outer: S2StringContextCreation =>
  def createExamplesGroup(i: Int): ExamplesGroup = {
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
    if      (i == 1 ) { g1  = () => g }
    else if (i == 2 ) { g2  = () => g }
    else if (i == 3 ) { g3  = () => g }
    else if (i == 4 ) { g4  = () => g }
    else if (i == 5 ) { g5  = () => g }
    else if (i == 6 ) { g6  = () => g }
    else if (i == 7 ) { g7  = () => g }
    else if (i == 8 ) { g8  = () => g }
    else if (i == 9 ) { g9  = () => g }
    else if (i == 10) { g10 = () => g }
    else if (i == 11) { g11 = () => g }
    else if (i == 12) { g12 = () => g }
    else if (i == 13) { g13 = () => g }
    else if (i == 14) { g14 = () => g }
    else if (i == 15) { g15 = () => g }
    else if (i == 16) { g16 = () => g }
    else if (i == 17) { g17 = () => g }
    else if (i == 18) { g18 = () => g }
    else if (i == 19) { g19 = () => g }
    else if (i == 20) { g20 = () => g }
    else if (i == 21) { g21 = () => g }
    else if (i == 22) { g22 = () => g }
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
trait Grouped extends GroupsLike { outer: S2StringContextCreation =>
  def createExamplesGroup(i: Int): ExamplesGroup = {
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

trait GroupsLike { this: S2StringContextCreation =>
  trait AutoNumberedGroup extends ExamplesGroup {
    private var autoNumberedExamples: Seq[ExecutionVar] = Seq()

    override def createExample(i: Int) =
      if (autoNumberedExamples.nonEmpty) autoNumberedExamples.applyOrElse(i, (index:Int) => new ExecutionVar())
      else                               numberedExamples(i)

    def eg = this

    def :=[R: AsResult](r: =>R) {
      autoNumberedExamples = autoNumberedExamples :+ ExecutionVar.result(r)
    }

    def :=[R : AsResult](f: Env => R) {
      autoNumberedExamples = autoNumberedExamples :+ ExecutionVar.withEnv(f)
    }

    def :=[R](f: ExecutionEnv => R)(implicit p: ImplicitParam, asResult: AsResult[R]) {
      autoNumberedExamples = autoNumberedExamples :+ ExecutionVar.withExecutionEnv(f)
    }

    def :=[R](f: ExecutorService => R)(implicit p1: ImplicitParam1, asResult: AsResult[R]) {
      autoNumberedExamples = autoNumberedExamples :+ ExecutionVar.withExecutorService(f)
    }
  }

  implicit def executionVarIsInterpolatedFragment(executionVar: =>ExecutionVar): InterpolatedFragment =
    createExecutionInterpolatedFragment(executionVar.execution())

  trait group extends AutoNumberedGroup

  def createExamplesGroup(i: Int): ExamplesGroup
}

case class ExamplesGroup(private var name: String = "") {
  def groupName = name

  def nameIs(n: String) = { name = s"'$n'"; this }

  val e1:  ExecutionVar = new ExecutionVar()
  val e2:  ExecutionVar = new ExecutionVar()
  val e3:  ExecutionVar = new ExecutionVar()
  val e4:  ExecutionVar = new ExecutionVar()
  val e5:  ExecutionVar = new ExecutionVar()
  val e6:  ExecutionVar = new ExecutionVar()
  val e7:  ExecutionVar = new ExecutionVar()
  val e8:  ExecutionVar = new ExecutionVar()
  val e9:  ExecutionVar = new ExecutionVar()
  val e10: ExecutionVar = new ExecutionVar()
  val e11: ExecutionVar = new ExecutionVar()
  val e12: ExecutionVar = new ExecutionVar()
  val e13: ExecutionVar = new ExecutionVar()
  val e14: ExecutionVar = new ExecutionVar()
  val e15: ExecutionVar = new ExecutionVar()
  val e16: ExecutionVar = new ExecutionVar()
  val e17: ExecutionVar = new ExecutionVar()
  val e18: ExecutionVar = new ExecutionVar()
  val e19: ExecutionVar = new ExecutionVar()
  val e20: ExecutionVar = new ExecutionVar()
  val e21: ExecutionVar = new ExecutionVar()
  val e22: ExecutionVar = new ExecutionVar()

  protected lazy val numberedExamples = Seq(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18,e19,e20,e21,e22)

  def createExample(i: Int) = numberedExamples(i)
}

class ExecutionVar(var execution: () => Execution = () => Execution.result(new execute.Pending(s" - PENDING"))) {
  def :=[T : AsResult](t: =>T) = {
    execution = () => Execution.result(t)
    this
  }

  def :=[R : AsResult](f: Env => R) = {
    execution = () => Execution.withEnv(f)
    this
  }

  def :=(other: ExecutionVar) = {
    execution = () => other.execution()
    this
  }
}
object ExecutionVar {
  def result[R : AsResult](r: =>R) =
    new ExecutionVar := r
  
  def withEnv[R : AsResult](f: Env => R) =
    new ExecutionVar(() => Execution.withEnv(f))

  def withExecutionEnv[R : AsResult](f: ExecutionEnv => R) =
    new ExecutionVar(() => Execution.withExecutionEnv(f))

  def withExecutorService[R : AsResult](f: ExecutorService => R) =
    new ExecutionVar(() => Execution.withExecutorService(f))

}
