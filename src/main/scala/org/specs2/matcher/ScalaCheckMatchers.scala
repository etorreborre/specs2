package org.specs2
package matcher

import org.scalacheck.util.StdRand
import org.scalacheck.Prop._
import org.scalacheck.Test.{ Params, Proved, Passed, Failed, Exhausted, GenException, PropException, Result }
import org.scalacheck.Pretty._
import scala.collection.Map
import io.{Output, ConsoleOutput}
import org.scalacheck._
import execute.{ResultLogicalCombinators, AsResult}

/**
 * The ScalaCheckMatchers trait provides matchers which allow to
 * assess properties multiple times with generated data.
 * @see the <a href="https://github.com/rickynils/scalacheck">ScalaCheck project</a>
 */
trait ScalaCheckMatchers extends ConsoleOutput with ScalaCheckParameters
   with FunctionPropertyImplicits
   with ResultPropertyImplicits
   with ResultLogicalCombinators
   with ApplicableArbitraries
   with Expectations { outer: ScalaCheckMatchers =>

  /** implicit typeclass instance to create examples from Props */
  implicit def propAsResult(implicit p: Parameters): AsResult[Prop] = new AsResult[Prop] {
    def asResult(prop: =>Prop): execute.Result = checkProp(prop)(p)
  }
  /** allow to combine properties as if they were results */
  implicit def combineProp(prop: =>Prop)(implicit p: Parameters): ResultLogicalCombinator = combineResult(propAsResult(p).asResult(prop))

  /**
   * transform a Function returning a MatchResult (or anything which can be converted to a Prop) as a ScalaCheck property
   */
  def prop[T, R](result: T => R)(implicit toProp: (=>R) => Prop, a: Arbitrary[T], s: Shrink[T]): Prop = check1(result)
  def propNoShrink[T, R](result: T => R)(implicit toProp: (=>R) => Prop, a: Arbitrary[T]): Prop = check1NoShrink(result)

  implicit def check1[T, R](result: T => R)(implicit toProp: (=>R) => Prop, a: Arbitrary[T], s: Shrink[T]): Prop = Prop.forAll((t: T) => toProp(result(t)))
  def check1NoShrink[T, R](result: T => R)(implicit toProp: (=>R) => Prop, a: Arbitrary[T]): Prop = Prop.forAllNoShrink(a.arbitrary)((t: T) => toProp(result(t)))

  def prop[T1, T2, R](result: (T1, T2) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2]): Prop = check2(result)
  def propNoShrink[T1, T2, R](result: (T1, T2) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2]): Prop = check2NoShrink(result)

  implicit def check2[T1, T2, R](result: (T1, T2) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2]): Prop =
    Prop.forAll((t1: T1, t2: T2) => toProp(result(t1, t2)))
  def check2NoShrink[T1, T2, R](result: (T1, T2) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2]): Prop =
    Prop.forAllNoShrink(a1.arbitrary, a2.arbitrary)((t1: T1, t2: T2) => toProp(result(t1, t2)))

  def prop[T1, T2, T3, R](result: (T1, T2, T3) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3]): Prop = check3(result)
  def propNoShrink[T1, T2, T3, R](result: (T1, T2, T3) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3]): Prop = check3NoShrink(result)

  implicit def check3[T1, T2, T3, R](result: (T1, T2, T3) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3]): Prop =
    Prop.forAll((t1: T1, t2: T2, t3: T3) => toProp(result(t1, t2, t3)))
  def check3NoShrink[T1, T2, T3, R](result: (T1, T2, T3) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3]): Prop =
    Prop.forAllNoShrink(a1.arbitrary, a2.arbitrary, a3.arbitrary)((t1: T1, t2: T2, t3: T3) => toProp(result(t1, t2, t3)))

  def prop[T1, T2, T3, T4, R](result: (T1, T2, T3, T4) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4]): Prop = check4(result)
  def propNoShrink[T1, T2, T3, T4, R](result: (T1, T2, T3, T4) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4]): Prop = check4NoShrink(result)

  implicit def check4[T1, T2, T3, T4, R](result: (T1, T2, T3, T4) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4]): Prop =
    Prop.forAll((t1: T1, t2: T2, t3: T3, t4: T4) => toProp(result(t1, t2, t3, t4)))
  def check4NoShrink[T1, T2, T3, T4, R](result: (T1, T2, T3, T4) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4]): Prop =
    Prop.forAllNoShrink(a1.arbitrary, a2.arbitrary, a3.arbitrary, a4.arbitrary)((t1: T1, t2: T2, t3: T3, t4: T4) => toProp(result(t1, t2, t3, t4)))

  def prop[T1, T2, T3, T4, T5, R](result: (T1, T2, T3, T4, T5) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5]): Prop = check5(result)
  def propNoShrink[T1, T2, T3, T4, T5, R](result: (T1, T2, T3, T4, T5) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5]): Prop = check5NoShrink(result)

  implicit def check5[T1, T2, T3, T4, T5, R](result: (T1, T2, T3, T4, T5) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5]): Prop =
    Prop.forAll((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5) => toProp(result(t1, t2, t3, t4, t5)))
  def check5NoShrink[T1, T2, T3, T4, T5, R](result: (T1, T2, T3, T4, T5) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5]): Prop =
    Prop.forAllNoShrink(a1.arbitrary, a2.arbitrary, a3.arbitrary, a4.arbitrary, a5.arbitrary)((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5) => toProp(result(t1, t2, t3, t4, t5)))

  def prop[T1, T2, T3, T4, T5, T6, R](result: (T1, T2, T3, T4, T5, T6) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1],a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5], a6: Arbitrary[T6], s6: Shrink[T6]): Prop = check6(result)
  def propNoShrink[T1, T2, T3, T4, T5, T6, R](result: (T1, T2, T3, T4, T5, T6) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6]): Prop = check6NoShrink(result)

  implicit def check6[T1, T2, T3, T4, T5, T6, R](result: (T1, T2, T3, T4, T5, T6) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1],a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5], a6: Arbitrary[T6], s6: Shrink[T6]): Prop =
    Prop.forAll((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6) => toProp(result(t1, t2, t3, t4, t5, t6)))
  def check6NoShrink[T1, T2, T3, T4, T5, T6, R](result: (T1, T2, T3, T4, T5, T6) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6]): Prop =
    Prop.forAllNoShrink(a1.arbitrary, a2.arbitrary, a3.arbitrary, a4.arbitrary, a5.arbitrary, a6.arbitrary)((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6) => toProp(result(t1, t2, t3, t4, t5, t6)))

  def prop[T1, T2, T3, T4, T5, T6, T7, R](result: (T1, T2, T3, T4, T5, T6, T7) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2],a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5], a6: Arbitrary[T6], s6: Shrink[T6], a7: Arbitrary[T7], s7: Shrink[T7]): Prop = check7(result)
  def propNoShrink[T1, T2, T3, T4, T5, T6, T7, R](result: (T1, T2, T3, T4, T5, T6, T7) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7]): Prop = check7NoShrink(result)

  implicit def check7[T1, T2, T3, T4, T5, T6, T7, R](result: (T1, T2, T3, T4, T5, T6, T7) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2],a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5], a6: Arbitrary[T6], s6: Shrink[T6], a7: Arbitrary[T7], s7: Shrink[T7]): Prop =
    Prop.forAll((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7) => toProp(result(t1, t2, t3, t4, t5, t6, t7)))
  def check7NoShrink[T1, T2, T3, T4, T5, T6, T7, R](result: (T1, T2, T3, T4, T5, T6, T7) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7]): Prop =
    Prop.forAllNoShrink(a1.arbitrary, a2.arbitrary, a3.arbitrary, a4.arbitrary, a5.arbitrary, a6.arbitrary, a7.arbitrary)((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7) => toProp(result(t1, t2, t3, t4, t5, t6, t7)))

  def prop[T1, T2, T3, T4, T5, T6, T7, T8, R](result: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5], a6: Arbitrary[T6], s6: Shrink[T6], a7: Arbitrary[T7], s7: Shrink[T7], a8: Arbitrary[T8], s8: Shrink[T8]): Prop = check8(result)
  def propNoShrink[T1, T2, T3, T4, T5, T6, T7, T8, R](result: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7], a8: Arbitrary[T8]): Prop = check8NoShrink(result)

  implicit def check8[T1, T2, T3, T4, T5, T6, T7, T8, R](result: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5], a6: Arbitrary[T6], s6: Shrink[T6], a7: Arbitrary[T7], s7: Shrink[T7], a8: Arbitrary[T8], s8: Shrink[T8]): Prop =
    Prop.forAll((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8) => toProp(result(t1, t2, t3, t4, t5, t6, t7, t8)))
  def check8NoShrink[T1, T2, T3, T4, T5, T6, T7, T8, R](result: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7], a8: Arbitrary[T8]): Prop =
    Prop.forAllNoShrink(a1.arbitrary, a2.arbitrary, a3.arbitrary, a4.arbitrary, a5.arbitrary, a6.arbitrary, a7.arbitrary, a8.arbitrary)((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8) => toProp(result(t1, t2, t3, t4, t5, t6, t7, t8)))

  /** execute a PartialFunction as a ScalaCheck property */
  def prop[T, S](f: PartialFunction[T, S])(implicit toProp: S => Prop, a: Arbitrary[T], s: Shrink[T]): Prop = checkPartial(f)
  def propNoShrink[T, S](f: PartialFunction[T, S])(implicit toProp: S => Prop, a: Arbitrary[T]): Prop = checkPartialNoShrink(f)

  implicit def checkPartial[T, S](f: PartialFunction[T, S])(implicit toProp: S => Prop, a: Arbitrary[T], s: Shrink[T]): Prop =
    PartialFunctionPropertyImplicits.partialFunctionToProp(f).forAll
  implicit def checkPartialNoShrink[T, S](f: PartialFunction[T, S])(implicit toProp: S => Prop, a: Arbitrary[T]): Prop =
    PartialFunctionPropertyImplicits.partialFunctionToProp(f).forAllNoShrink

  /** execute a ScalaCheck property */
  def check(prop: Prop)(implicit p: Parameters): execute.Result = checkProp(prop)(p)

  /** execute a ScalaCheck property */
  implicit def checkProp(prop: Prop)(implicit p: Parameters): execute.Result =
    checkResultFailure(checkProperty(prop)(p))

  /**
   * checks if the property is true for each generated value, and with the specified
   * generation parameters `p`. `p` is transformed into a scalacheck parameters
   */
  private[specs2] def checkProperty(prop: Prop)(implicit p: Parameters): execute.Result = {
    checkScalaCheckProperty(prop)(p.toScalaCheckParameters(defaultPrettyParams))
  }

  /**
   * checks if the property is true for each generated value, and with the specified
   * scalacheck parameters. If verbose is true, then print the results on the console
   */
  private [matcher] def checkScalaCheckProperty(prop: =>Prop)(params: Test.Parameters): execute.Result = {
    // check the property with ScalaCheck
    val result = Test.check(params, prop)

    def counterExampleMessage(args: Prop.Args, n: Int, labels: Set[String]) =
      "A counter-example is "+counterExample(args)+" (" + afterNTries(n) + afterNShrinks(args) + ")" + failedLabels(labels)

    result match {
      case Result(Proved(as), succeeded, discarded, fq, _) =>
        execute.Success(noCounterExample(succeeded), frequencies(fq)(defaultPrettyParams), succeeded)
      case Result(Passed, succeeded, discarded, fq, _)     =>
        execute.Success(noCounterExample(succeeded), frequencies(fq)(defaultPrettyParams), succeeded)
      case r @ Result(GenException(execute.FailureException(f)), n, _, fq, _) => f
      case r @ Result(GenException(e), n, _, fq, _)        =>
        execute.Failure(prettyTestRes(r)(defaultPrettyParams) + frequencies(fq), e.getMessage(), e.getStackTrace().toList)
      case r @ Result(Exhausted, n, _, fq, _)              =>
        execute.Failure(prettyTestRes(r)(defaultPrettyParams) + frequencies(fq))
      case Result(Failed(args, labels), n, _, fq, _) =>
        new execute.Failure(counterExampleMessage(args, n, labels) + frequencies(fq)) {
          // the location is already included in the failure message
          override def location = ""
        }
      case Result(PropException(args, ex, labels), n, _, fq, _) =>
        ex match {
          case execute.FailureException(f) =>
            new execute.Failure(counterExampleMessage(args, n, labels+f.message) + frequencies(fq)) {
              override def location = f.location
            }
          case execute.SkipException(s)    => s
          case execute.PendingException(p) => p
          case e: java.lang.Exception      =>
            execute.Error("A counter-example is "+counterExample(args)+": " + ex + getCause(e) +
                                                " ("+afterNTries(n)+")"+ failedLabels(labels) + frequencies(fq), e)
          case throwable    => throw ex
        }

    }
  }
  // depending on the result, return the appropriate success status and messages
  // the failure message indicates a counter-example to the property
  private[matcher] def noCounterExample(n: Int) = "The property passed without any counter-example " + afterNTries(n)
  private[matcher] def afterNTries(n: Int) = "after " + (if (n <= 1) n + " try" else n + " tries")
  private[matcher] def afterNShrinks(args: List[Arg[_]]) = {
    if (args.forall(_.shrinks == 0))  ""
    else
      args.map { arg =>
        if (arg.origArg != arg.arg) "'"+arg.origArg +"' -> '"+arg.arg+"'"
        else " = "
     }.mkString(" - shrinked (", ",", ")")
  }
  /** @return the cause of the exception as a String if there is one */
  private[matcher] def getCause(e: java.lang.Exception) = Option(e.getCause).map(c => "(caused by "+c+")").getOrElse("")

  private [matcher] def counterExample(args: List[Arg[_]]) = {
    if (args.size == 1)
      args.map(a => if (a.arg == null) "null" else a.arg.toString).mkString("'", "", "'")
    else if (args.exists(_.arg.toString.isEmpty))
      args.map(_.arg).mkString("['", "', '", "']")
    else
      args.map(_.arg).mkString("[", ", ", "]")
  }
  private [matcher] def failedLabels(labels: Set[String]) = {
    if (labels.isEmpty)  ""  
    else labels.mkString("\n", ", ", "\n")
  }

  private[matcher] def frequencies(fq: Prop.FM)(implicit params: Pretty.Params) = {
    if (fq.getRatios.isEmpty) ""
    else "\n" + Pretty.prettyFreqMap(fq)(params)
  }
}
object ScalaCheckMatchers extends ScalaCheckMatchers
/**
 * This trait adds some syntactic sugar to transform function
 * to properties by appending forAll
 */
trait FunctionPropertyImplicits {
  /** transform a function returning a boolean to a property by appending forAll */
  implicit def functionToProp[T](f: T => Boolean)(implicit a: Arbitrary[T], s: Shrink[T]): Prop = functionToForAll(f).forAll
  implicit def functionToForAll[T](f: T => Boolean)(implicit a: Arbitrary[T], s: Shrink[T]): FunctionForAll[T] = new FunctionForAll(f)(a, s)
  class FunctionForAll[T](f: T => Boolean)(implicit a: Arbitrary[T], s: Shrink[T]) {
    def forAll: Prop = Prop.forAll(f)
  }
  /** transform a function returning a boolean to a property by appending forAll */
  implicit def functionToProp2[T1, T2](f: (T1, T2) => Boolean): FunctionForAll2[T1, T2] = new FunctionForAll2(f)
  class FunctionForAll2[T1, T2](f: (T1, T2) => Boolean) {
    def forAll(implicit
      a1: Arbitrary[T1], s1: Shrink[T1],
      a2: Arbitrary[T2], s2: Shrink[T2]
      ): Prop = Prop.forAll(f)
  }
  /** transform a function returning a boolean to a property by appending forAll */
  implicit def functionToProp3[T1, T2, T3](f: (T1, T2, T3) => Boolean): FunctionForAll3[T1, T2, T3] = new FunctionForAll3(f)
  class FunctionForAll3[T1, T2, T3](f: (T1, T2, T3) => Boolean) {
    def forAll(implicit
      a1: Arbitrary[T1], s1: Shrink[T1],
      a2: Arbitrary[T2], s2: Shrink[T2],
      a3: Arbitrary[T3], s3: Shrink[T3]
      ): Prop = Prop.forAll(f)
  }
}
/**
 * This trait adds some syntactic sugar to transform partial functions to properties by appending forAll
 */
trait PartialFunctionPropertyImplicits {
  /** transform a partial function returning a boolean to a property by appending forAll */
  implicit def partialFunctionToProp[T, S](f: PartialFunction[T, S]): PartialFunctionForAll[T, S] = new PartialFunctionForAll(f)
  class PartialFunctionForAll[T, S](f: PartialFunction[T, S]) {
    def forAll(implicit toProp: S => Prop, a: Arbitrary[T], s: Shrink[T]): Prop = Prop.forAll(f)
    def forAllNoShrink(implicit toProp: S => Prop, a: Arbitrary[T]): Prop = Prop.forAllNoShrink(a.arbitrary)(f)
  }
}
object PartialFunctionPropertyImplicits extends PartialFunctionPropertyImplicits

trait ResultPropertyImplicits {

  implicit def unitToProp(u: =>Unit): Prop = booleanToProp({u; true})
  implicit def propToProp(p: =>Prop): Prop = p
  implicit def booleanToProp(b: =>Boolean): Prop = resultProp(if (b) execute.Success() else execute.Failure())
  implicit def callByNameMatchResultToProp[T](m: =>MatchResult[T]): Prop = resultProp(m.toResult)
  implicit def matchResultToProp[T](m: MatchResult[T]): Prop = resultProp(m.toResult)

  implicit def resultProp(r: =>execute.Result): Prop = {
    new Prop {
      def apply(params: Prop.Params) = {
        lazy val result = execute.ResultExecution.execute(r)
        val prop = 
        result match {
          case f : execute.Failure => Prop.falsified :| (f.message+" ("+f.location+")")
          case s : execute.Skipped => Prop.exception(new execute.SkipException(s))
          case p : execute.Pending => Prop.exception(new execute.PendingException(p))
          case e : execute.Error   => Prop.exception(e.exception)
          case other               => Prop.passed
        }
        prop.apply(params)
      }
    }
  }
}

/**
 * This trait enables some syntactic sugar when it is necessary to pass several arbitrary instances
 */
trait ApplicableArbitraries { this: ScalaCheckMatchers =>

  implicit def applicableArbitrary[T](a: Arbitrary[T]): ApplicableArbitrary[T] = ApplicableArbitrary(a)
  case class ApplicableArbitrary[T](a: Arbitrary[T]) {
    def apply[R](f: T => R)(implicit toProp: (=>R) => Prop, s: Shrink[T]) = prop(f)(toProp, a, s)
  }
  implicit def applicableArbitrary2[T1, T2](a: (Arbitrary[T1], Arbitrary[T2])) = ApplicableArbitrary2(a._1, a._2)
  case class ApplicableArbitrary2[T1, T2](a1: Arbitrary[T1], a2: Arbitrary[T2]) {
    def apply[R](f: (T1, T2) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2]) = prop(f)(toProp, a1, s1, a2, s2)
  }
  implicit def applicableArbitrary3[T1, T2, T3](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3])) = ApplicableArbitrary3(a._1, a._2, a._3)
  case class ApplicableArbitrary3[T1, T2, T3](a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3]) {
    def apply[R](f: (T1, T2, T3) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3]) = prop(f)(toProp, a1, s1, a2, s2, a3, s3)
  }
  implicit def applicableArbitrary4[T1, T2, T3, T4](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3], Arbitrary[T4])) = ApplicableArbitrary4(a._1, a._2, a._3, a._4)
  case class ApplicableArbitrary4[T1, T2, T3, T4](a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4]) {
    def apply[R](f: (T1, T2, T3, T4) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4]) = prop(f)(toProp, a1, s1, a2, s2, a3, s3, a4, s4)
  }
  implicit def applicableArbitrary5[T1, T2, T3, T4, T5](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3], Arbitrary[T4], Arbitrary[T5])) = ApplicableArbitrary5(a._1, a._2, a._3, a._4, a._5)
  case class ApplicableArbitrary5[T1, T2, T3, T4, T5](a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5]) {
    def apply[R](f: (T1, T2, T3, T4, T5) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4], s5: Shrink[T5]) = prop(f)(toProp, a1, s1, a2, s2, a3, s3, a4, s4, a5, s5)
  }
  implicit def applicableArbitrary6[T1, T2, T3, T4, T5, T6](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3], Arbitrary[T4], Arbitrary[T5], Arbitrary[T6])) = ApplicableArbitrary6(a._1, a._2, a._3, a._4, a._5, a._6)
  case class ApplicableArbitrary6[T1, T2, T3, T4, T5, T6](a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6]) {
    def apply[R](f: (T1, T2, T3, T4, T5, T6) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4], s5: Shrink[T5], s6: Shrink[T6]) = prop(f)(toProp, a1, s1, a2, s2, a3, s3, a4, s4, a5, s5, a6, s6)
  }
  implicit def applicableArbitrary7[T1, T2, T3, T4, T5, T6, T7](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3], Arbitrary[T4], Arbitrary[T5], Arbitrary[T6], Arbitrary[T7])) = ApplicableArbitrary7(a._1, a._2, a._3, a._4, a._5, a._6, a._7)
  case class ApplicableArbitrary7[T1, T2, T3, T4, T5, T6, T7](a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7]) {
    def apply[R](f: (T1, T2, T3, T4, T5, T6, T7) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4], s5: Shrink[T5], s6: Shrink[T6], s7: Shrink[T7]) = prop(f)(toProp, a1, s1, a2, s2, a3, s3, a4, s4, a5, s5, a6, s6, a7, s7)
  }
  implicit def applicableArbitrary8[T1, T2, T3, T4, T5, T6, T7, T8](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3], Arbitrary[T4], Arbitrary[T5], Arbitrary[T6], Arbitrary[T7], Arbitrary[T8])) = ApplicableArbitrary8(a._1, a._2, a._3, a._4, a._5, a._6, a._7, a._8)
  case class ApplicableArbitrary8[T1, T2, T3, T4, T5, T6, T7, T8](a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7], a8: Arbitrary[T8]) {
    def apply[R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4], s5: Shrink[T5], s6: Shrink[T6], s7: Shrink[T7], s8: Shrink[T8]) = prop(f)(toProp, a1, s1, a2, s2, a3, s3, a4, s4, a5, s5, a6, s6, a7, s7, a8, s8)
  }

}
/**
 * This trait provides generation parameters to use with the `ScalaCheckMatchers`
 */
trait ScalaCheckParameters { outer: ScalaCheckMatchers with Output =>
  /**
   * default parameters. Uses ScalaCheck default values and doesn't print anything to the console
   */
  implicit def defaultParameters = new Parameters()
  /** default parameters to display pretty messages */
  implicit def defaultPrettyParams = Pretty.defaultParams

  /** set specific execution parameters on a Property */
  implicit def setProperty(p: Prop) = new SetProperty(p)
  class SetProperty(prop: Prop) {
    /** create parameters with verbose = false */
    def set(minTestsOk: Int             = defaultParameters.minTestsOk,
            minSize: Int                = defaultParameters.minSize,
            maxDiscardRatio: Float      = defaultParameters.maxDiscardRatio,
            maxSize: Int                = defaultParameters.maxSize,
            workers: Int                = defaultParameters.workers,
            rng: java.util.Random       = defaultParameters.rng,
            callback: Test.TestCallback = defaultParameters.callback,
            loader: Option[ClassLoader] = defaultParameters.loader): execute.Result =
      check(prop)(new Parameters(minTestsOk, minSize, maxDiscardRatio, maxSize, workers, rng, callback, loader, verbose = false, outer))
  }

  /** set specific execution parameters on a Property */
  implicit def displayProperty(p: Prop) = new DisplayProperty(p)
  class DisplayProperty(prop: Prop) {
    /** create parameters with verbose = true */
    def display(minTestsOk: Int             = defaultParameters.minTestsOk,
                minSize: Int                = defaultParameters.minSize,
                maxDiscardRatio: Float      = defaultParameters.maxDiscardRatio,
                maxSize: Int                = defaultParameters.maxSize,
                workers: Int                = defaultParameters.workers,
                rng: java.util.Random       = defaultParameters.rng,
                callback: Test.TestCallback = defaultParameters.callback,
                loader: Option[ClassLoader] = defaultParameters.loader): execute.Result =
      check(prop)(new Parameters(minTestsOk, minSize, maxDiscardRatio, maxSize, workers, rng, callback, loader, verbose = true, outer))
  }

  /** create parameters with verbose = false */
  def set(minTestsOk: Int             = defaultParameters.minTestsOk,
          minSize: Int                = defaultParameters.minSize,
          maxDiscardRatio: Float      = defaultParameters.maxDiscardRatio,
          maxSize: Int                = defaultParameters.maxSize,
          workers: Int                = defaultParameters.workers,
          rng: java.util.Random       = defaultParameters.rng,
          callback: Test.TestCallback = defaultParameters.callback,
          loader: Option[ClassLoader] = defaultParameters.loader): Parameters =
    new Parameters(minTestsOk, minSize, maxDiscardRatio, maxSize, workers, rng, callback, loader, verbose = false, outer)

  /** create parameters with verbose = true */
  def display(minTestsOk: Int             = defaultParameters.minTestsOk,
              minSize: Int                = defaultParameters.minSize,
              maxDiscardRatio: Float      = defaultParameters.maxDiscardRatio,
              maxSize: Int                = defaultParameters.maxSize,
              workers: Int                = defaultParameters.workers,
              rng: java.util.Random       = defaultParameters.rng,
              callback: Test.TestCallback = defaultParameters.callback,
              loader: Option[ClassLoader] = defaultParameters.loader): Parameters =
    new Parameters(minTestsOk, minSize, maxDiscardRatio, maxSize, workers, rng, callback, loader, verbose = true, outer)

}

/**
* This class is the base class for the display and set case classes.<br>
* It contains a Map of generation parameters and indicates if the generation
* must be verbose.
*/
case class Parameters(minTestsOk: Int             = Test.Parameters.default.minSuccessfulTests,
                      minSize: Int                = Test.Parameters.default.minSize,
                      maxDiscardRatio: Float      = Test.Parameters.default.maxDiscardRatio,
                      maxSize: Int                = Test.Parameters.default.maxSize,
                      workers: Int                = Test.Parameters.default.workers,
                      rng: java.util.Random       = Test.Parameters.default.rng,
                      callback: Test.TestCallback = Test.Parameters.default.testCallback,
                      loader: Option[ClassLoader] = Test.Parameters.default.customClassLoader,
                      verbose: Boolean            = false,
                      output: Output              = ConsoleOutput) { outer =>

  def testCallback(implicit pretty: Pretty.Params) = if (verbose) verboseCallback.chain(callback) else callback

  def toScalaCheckParameters(implicit pretty: Pretty.Params): Test.Parameters =
    new Test.Parameters {
      def minSuccessfulTests = outer.minTestsOk
      def maxDiscardRatio    = outer.maxDiscardRatio
      def maxSize            = outer.maxSize
      def minSize            = outer.minSize
      def workers            = outer.workers
      def rng                = outer.rng
      def testCallback       = outer.testCallback
      def customClassLoader  = outer.loader
    }

  def verboseCallback(implicit pretty: Pretty.Params) = new Test.TestCallback {
    override def onPropEval(name: String, threadXdx: Int, succeeded: Int, discarded: Int): Unit = {
      if (discarded == 0) output.printf("\rPassed %d tests", succeeded)
      else                output.printf("\rPassed %d tests; %d discarded", succeeded, discarded)
    }
    override def onTestResult(name: String, result: Test.Result) = {
      val s = prettyTestRes(result)(pretty)
      output.printf("\r%s %s%s\n", if (result.passed) "+" else "!", s, List.fill(70 - s.length)(" ").mkString(""))
    }
  }

}


