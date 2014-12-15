package org.specs2
package matcher

import org.scalacheck.Prop._
import org.scalacheck.Test.{Exhausted, Failed, Passed, PropException, Proved, Result}
import org.scalacheck._
import org.scalacheck.util.Pretty._
import org.scalacheck.util.{FreqMap, Pretty}
import org.specs2.execute.{AsResult, ResultLogicalCombinators}
import org.specs2.io.{ConsoleOutput, Output}
import org.specs2.text.NotNullStrings._

/**
 * The ScalaCheckMatchers trait provides matchers which allow to
 * assess properties multiple times with generated data.
 * @see the <a href="https://github.com/rickynils/scalacheck">ScalaCheck project</a>
 */
trait ScalaCheckMatchers extends ConsoleOutput with ScalaCheckParameters
   with FunctionAsProperty
   with ResultPropertyImplicits
   with ResultLogicalCombinators
   with ApplicableArbitraries
   with Expectations { outer: ScalaCheckMatchers =>

  /** implicit typeclass instance to create examples from Props */
  implicit def propAsResult[P <: Prop](implicit p: Parameters): AsResult[P] = new AsResult[P] {
    def asResult(prop: =>P): execute.Result = checkProp(prop)(p)
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
    PartialFunctionAsProp.PartialFunctionForAll(f).forAll

  implicit def checkPartialNoShrink[T, S](f: PartialFunction[T, S])(implicit toProp: S => Prop, a: Arbitrary[T]): Prop =
    PartialFunctionAsProp.PartialFunctionForAll(f).forAllNoShrink

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
    checkScalaCheckProperty(prop)(p.toScalaCheckParameters)
  }

  /**
   * checks if the property is true for each generated value, and with the specified
   * scalacheck parameters. If verbose is true, then print the results on the console
   */
  private [matcher] def checkScalaCheckProperty(prop: =>Prop)(params: Test.Parameters): execute.Result = {
    // check the property with ScalaCheck
    val result = Test.check(params, prop)

    def counterExampleMessage(args: List[Prop.Arg[Any]], n: Int, labels: Set[String]) =
      "A counter-example is "+counterExample(args)+" (" + afterNTries(n) + afterNShrinks(args) + ")" + failedLabels(labels)

    result match {
      case Result(Proved(as), succeeded, discarded, fq, _) =>
        execute.Success(noCounterExample(succeeded), frequencies(fq)(defaultPrettyParams), succeeded)

      case Result(Passed, succeeded, discarded, fq, _)     =>
        execute.Success(noCounterExample(succeeded), frequencies(fq)(defaultPrettyParams), succeeded)

      case r @ Result(Exhausted, n, _, fq, _)              =>
        execute.Failure(prettyTestRes(r)(defaultPrettyParams) + frequencies(fq))

      case Result(Failed(args, labels), n, _, fq, _) =>
        new execute.Failure(counterExampleMessage(args, n, labels) + frequencies(fq), details = collectDetails(fq)) {
          // the location is already included in the failure message
          override def location = ""
        }

      case Result(PropException(args, ex, labels), n, _, fq, _) =>
        ex match {
          case execute.FailureException(f) =>
            new execute.Failure(counterExampleMessage(args, n, labels+f.message) + frequencies(fq), details = f.details) {
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

  /**
   * Failure details might get collected with the collect operation when evaluating
   * the Prop
   */
  private def collectDetails[T](fq: FreqMap[Set[T]]): execute.Details = {
    fq.getRatios.flatMap(_._1.toList).collect {
      case d : execute.FailureDetails    => d
      case d : execute.FailureSeqDetails => d
      case d : execute.FailureSetDetails => d
      case d : execute.FailureMapDetails => d
    }.headOption.getOrElse(execute.NoDetails)
  }

  /**
   * Remove failure details from collected data
   */
  private def removeDetails(fq: FreqMap[Set[Any]]): FreqMap[Set[Any]] = {
    fq.getCounts.foldLeft(FreqMap.empty[Set[Any]]) { case (res, (set, count)) =>
      set.toList match {
        case (_:execute.FailureDetails) :: _    => res
        case (_:execute.FailureSeqDetails) :: _ => res
        case (_:execute.FailureSetDetails) :: _ => res
        case (_:execute.FailureMapDetails) :: _ => res
        case _                                  => (1 to count).foldLeft(res) { case (map, i) => map + set }
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
      args.map(a => a.arg.notNull).mkString("'", "", "'")
    else if (args.exists(_.arg.notNull.isEmpty))
      args.map(_.arg.notNull).mkString("['", "', '", "']")
    else
      args.map(_.arg.notNull).mkString("[", ", ", "]")
  }
  private [matcher] def failedLabels(labels: Set[String]) = {
    if (labels.isEmpty)  ""  
    else labels.mkString("\n", ", ", "\n")
  }

  private[matcher] def frequencies(fq: FreqMap[Set[Any]])(implicit params: Pretty.Params) = {
    if (fq.getRatios.isEmpty) ""
    else "\n" + Pretty.prettyFreqMap(fq)(params)
  }
}
object ScalaCheckMatchers extends ScalaCheckMatchers



