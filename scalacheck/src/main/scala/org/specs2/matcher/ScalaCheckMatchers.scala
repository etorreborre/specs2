package org.specs2
package matcher

import org.scalacheck.Prop._
import org.scalacheck.Test.Result
import org.scalacheck.Test._
import org.scalacheck.util.{ FreqMap, Pretty }
import Pretty._
import org.specs2.reporter.NoStdOut
import scala.collection.Map
import io.{Output, ConsoleOutput}
import org.scalacheck._
import execute.{ResultLogicalCombinators, AsResult}
import text.NotNullStrings._

import scala.math._

/**
 * The ScalaCheckMatchers trait provides matchers which allow to
 * assess properties multiple times with generated data.
 * @see the <a href="https://github.com/rickynils/scalacheck">ScalaCheck project</a>
 */
trait ScalaCheckMatchers extends ConsoleOutput with ScalaCheckParameters
   with PropertiesCreation
   with CheckProperty
   with FunctionPropertyImplicits
   with ResultPropertyImplicits
   with ResultLogicalCombinators
   with ApplicableArbitraries
   with Expectations {
  outer: ScalaCheckMatchers =>

  /** implicit typeclass instance to create examples from Props */
  implicit def propAsResult[P <: Prop](implicit p: Parameters): AsResult[P] = new AsResult[P] {
    def asResult(prop: => P): execute.Result = checkProp(prop)(p)
  }

  /** allow to combine properties as if they were results */
  implicit def combineProp(prop: => Prop)(implicit p: Parameters): ResultLogicalCombinator = combineResult(propAsResult(p).asResult(prop))


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
    checkScalaCheckProperty(prop)(p)
  }
}

trait CheckProperty {
  /** default parameters to display pretty messages */
  implicit def defaultPrettyParams = Pretty.defaultParams

  /**
   * checks if the property is true for each generated value, and with the specified
   * scalacheck parameters. If verbose is true, then print the results on the console
   */
  private [matcher] def checkScalaCheckProperty(prop: =>Prop)(implicit parameters: Parameters): execute.Result = {
    // check the property with ScalaCheck
    val result = Test.check(parameters.toScalaCheckParameters(defaultPrettyParams), prop)

    def counterExampleMessage(args: List[Prop.Arg[Any]], n: Int, labels: Set[String]) =
      "A counter-example is "+counterExample(args)+" (" + afterNTries(n) + afterNShrinks(args) + ")" + failedLabels(labels)

    result match {
      case Result(Proved(as), succeeded, discarded, fq, _) =>
        execute.Success(noCounterExample(succeeded), frequencies(fq), succeeded)

      case Result(Passed, succeeded, discarded, fq, _)     =>
        execute.Success(noCounterExample(succeeded), frequencies(fq), succeeded)

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

  private[matcher] def frequencies(fq: FreqMap[Set[Any]])(implicit parameters: Parameters, prettyParams: Pretty.Params) = {
    if (parameters.verbose) {
      if (fq.getRatios.isEmpty) ""
      else "\n" + parameters.prettyCollected.pretty(removeDetails(fq))(prettyParams)
    } else ""
  }
}
object ScalaCheckMatchers extends ScalaCheckMatchers

/**
 * Properties creation
 */
trait PropertiesCreation {
  /** collect data if the collect parameter is true */
  def collectData[T](t: T)(prop: Prop)(implicit params: Parameters) = if (params.collect) collect(t)(prop) else prop
  def collectData[T1, T2](t1: T1, t2: T2)(prop: Prop)(implicit params: Parameters) = if (params.collect) collect(t1)(collect(t2)(prop)) else prop
  def collectData[T1, T2, T3](t1: T1, t2: T2, t3: T3)(prop: Prop)(implicit params: Parameters) = if (params.collect) collect(t1)(collect(t2)(collect(t3)(prop))) else prop
  def collectData[T1, T2, T3, T4](t1: T1, t2: T2, t3: T3, t4: T4)(prop: Prop)(implicit params: Parameters) = if (params.collect) collect(t1)(collect(t2)(collect(t3)(collect(t4)(prop)))) else prop
  def collectData[T1, T2, T3, T4, T5](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5)(prop: Prop)(implicit params: Parameters) = if (params.collect) collect(t1)(collect(t2)(collect(t3)(collect(t4)(collect(t5)(prop))))) else prop
  def collectData[T1, T2, T3, T4, T5, T6](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6)(prop: Prop)(implicit params: Parameters) = if (params.collect) collect(t1)(collect(t2)(collect(t3)(collect(t4)(collect(t5)(collect(t6)(prop)))))) else prop
  def collectData[T1, T2, T3, T4, T5, T6, T7](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7)(prop: Prop)(implicit params: Parameters) = if (params.collect) collect(t1)(collect(t2)(collect(t3)(collect(t4)(collect(t5)(collect(t6)(collect(t7)(prop))))))) else prop
  def collectData[T1, T2, T3, T4, T5, T6, T7, T8](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8)(prop: Prop)(implicit params: Parameters) = if (params.collect) collect(t1)(collect(t2)(collect(t3)(collect(t4)(collect(t5)(collect(t6)(collect(t7)(collect(t8)(prop)))))))) else prop

  /**
   * transform a Function returning a MatchResult (or anything which can be converted to a Prop) as a ScalaCheck property
   */
  def prop[T, R](result: T => R)(implicit toProp: (=>R) => Prop, a: Arbitrary[T], s: Shrink[T], p: Parameters): Prop = check1(result)
  def propNoShrink[T, R](result: T => R)(implicit toProp: (=>R) => Prop, a: Arbitrary[T], p: Parameters): Prop = check1NoShrink(result)

  implicit def check1[T, R](result: T => R)(implicit toProp: (=>R) => Prop, a: Arbitrary[T], s: Shrink[T], p: Parameters): Prop =
    Prop.forAll((t: T) => collectData(t)(toProp(result(t))))
  def check1NoShrink[T, R](result: T => R)(implicit toProp: (=>R) => Prop, a: Arbitrary[T], p: Parameters): Prop =
    Prop.forAllNoShrink(a.arbitrary)((t: T) => collectData(t)(toProp(result(t))))

  def prop[T1, T2, R](result: (T1, T2) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], p: Parameters): Prop = check2(result)
  def propNoShrink[T1, T2, R](result: (T1, T2) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], p: Parameters): Prop = check2NoShrink(result)

  implicit def check2[T1, T2, R](result: (T1, T2) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], p: Parameters): Prop =
    Prop.forAll((t1: T1, t2: T2) => collectData(t1, t2)(toProp(result(t1, t2))))
  def check2NoShrink[T1, T2, R](result: (T1, T2) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], p: Parameters): Prop =
    Prop.forAllNoShrink(a1.arbitrary, a2.arbitrary)((t1: T1, t2: T2) => collectData(t1, t2)(toProp(result(t1, t2))))

  def prop[T1, T2, T3, R](result: (T1, T2, T3) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], p: Parameters): Prop = check3(result)
  def propNoShrink[T1, T2, T3, R](result: (T1, T2, T3) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], p: Parameters): Prop = check3NoShrink(result)

  implicit def check3[T1, T2, T3, R](result: (T1, T2, T3) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], p: Parameters): Prop =
    Prop.forAll((t1: T1, t2: T2, t3: T3) => collectData(t1, t2, t3)(toProp(result(t1, t2, t3))))
  def check3NoShrink[T1, T2, T3, R](result: (T1, T2, T3) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], p: Parameters): Prop =
    Prop.forAllNoShrink(a1.arbitrary, a2.arbitrary, a3.arbitrary)((t1: T1, t2: T2, t3: T3) => collectData(t1, t2, t3)(toProp(result(t1, t2, t3))))

  def prop[T1, T2, T3, T4, R](result: (T1, T2, T3, T4) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], p: Parameters): Prop = check4(result)
  def propNoShrink[T1, T2, T3, T4, R](result: (T1, T2, T3, T4) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], p: Parameters): Prop = check4NoShrink(result)

  implicit def check4[T1, T2, T3, T4, R](result: (T1, T2, T3, T4) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], p: Parameters): Prop =
    Prop.forAll((t1: T1, t2: T2, t3: T3, t4: T4) => collectData(t1, t2, t3, t4)(toProp(result(t1, t2, t3, t4))))
  def check4NoShrink[T1, T2, T3, T4, R](result: (T1, T2, T3, T4) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], p: Parameters): Prop =
    Prop.forAllNoShrink(a1.arbitrary, a2.arbitrary, a3.arbitrary, a4.arbitrary)((t1: T1, t2: T2, t3: T3, t4: T4) => collectData(t1, t2, t3, t4)(toProp(result(t1, t2, t3, t4))))

  def prop[T1, T2, T3, T4, T5, R](result: (T1, T2, T3, T4, T5) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5], p: Parameters): Prop = check5(result)
  def propNoShrink[T1, T2, T3, T4, T5, R](result: (T1, T2, T3, T4, T5) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], p: Parameters): Prop = check5NoShrink(result)

  implicit def check5[T1, T2, T3, T4, T5, R](result: (T1, T2, T3, T4, T5) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5], p: Parameters): Prop =
    Prop.forAll((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5) => collectData(t1, t2, t3, t4, t5)(toProp(result(t1, t2, t3, t4, t5))))
  def check5NoShrink[T1, T2, T3, T4, T5, R](result: (T1, T2, T3, T4, T5) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], p: Parameters): Prop =
    Prop.forAllNoShrink(a1.arbitrary, a2.arbitrary, a3.arbitrary, a4.arbitrary, a5.arbitrary)((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5) => collectData(t1, t2, t3, t4, t5)(toProp(result(t1, t2, t3, t4, t5))))

  def prop[T1, T2, T3, T4, T5, T6, R](result: (T1, T2, T3, T4, T5, T6) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1],a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5], a6: Arbitrary[T6], s6: Shrink[T6], p: Parameters): Prop = check6(result)
  def propNoShrink[T1, T2, T3, T4, T5, T6, R](result: (T1, T2, T3, T4, T5, T6) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6], p: Parameters): Prop = check6NoShrink(result)

  implicit def check6[T1, T2, T3, T4, T5, T6, R](result: (T1, T2, T3, T4, T5, T6) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1],a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5], a6: Arbitrary[T6], s6: Shrink[T6], p: Parameters): Prop =
    Prop.forAll((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6) => collectData(t1, t2, t3, t4, t5, t6)(toProp(result(t1, t2, t3, t4, t5, t6))))
  def check6NoShrink[T1, T2, T3, T4, T5, T6, R](result: (T1, T2, T3, T4, T5, T6) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6], p: Parameters): Prop =
    Prop.forAllNoShrink(a1.arbitrary, a2.arbitrary, a3.arbitrary, a4.arbitrary, a5.arbitrary, a6.arbitrary)((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6) => collectData(t1, t2, t3, t4, t5, t6)(toProp(result(t1, t2, t3, t4, t5, t6))))

  def prop[T1, T2, T3, T4, T5, T6, T7, R](result: (T1, T2, T3, T4, T5, T6, T7) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2],a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5], a6: Arbitrary[T6], s6: Shrink[T6], a7: Arbitrary[T7], s7: Shrink[T7], p: Parameters): Prop = check7(result)
  def propNoShrink[T1, T2, T3, T4, T5, T6, T7, R](result: (T1, T2, T3, T4, T5, T6, T7) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7], p: Parameters): Prop = check7NoShrink(result)

  implicit def check7[T1, T2, T3, T4, T5, T6, T7, R](result: (T1, T2, T3, T4, T5, T6, T7) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2],a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5], a6: Arbitrary[T6], s6: Shrink[T6], a7: Arbitrary[T7], s7: Shrink[T7], p: Parameters): Prop =
    Prop.forAll((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7) => collectData(t1, t2, t3, t4, t5, t6, t7)(toProp(result(t1, t2, t3, t4, t5, t6, t7))))
  def check7NoShrink[T1, T2, T3, T4, T5, T6, T7, R](result: (T1, T2, T3, T4, T5, T6, T7) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7], p: Parameters): Prop =
    Prop.forAllNoShrink(a1.arbitrary, a2.arbitrary, a3.arbitrary, a4.arbitrary, a5.arbitrary, a6.arbitrary, a7.arbitrary)((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7) => collectData(t1, t2, t3, t4, t5, t6, t7)(toProp(result(t1, t2, t3, t4, t5, t6, t7))))

  def prop[T1, T2, T3, T4, T5, T6, T7, T8, R](result: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5], a6: Arbitrary[T6], s6: Shrink[T6], a7: Arbitrary[T7], s7: Shrink[T7], a8: Arbitrary[T8], s8: Shrink[T8], p: Parameters): Prop = check8(result)
  def propNoShrink[T1, T2, T3, T4, T5, T6, T7, T8, R](result: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7], a8: Arbitrary[T8], p: Parameters): Prop = check8NoShrink(result)

  implicit def check8[T1, T2, T3, T4, T5, T6, T7, T8, R](result: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5], a6: Arbitrary[T6], s6: Shrink[T6], a7: Arbitrary[T7], s7: Shrink[T7], a8: Arbitrary[T8], s8: Shrink[T8], p: Parameters): Prop =
    Prop.forAll((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8) => collectData(t1, t2, t3, t4, t5, t6, t7, t8)(toProp(result(t1, t2, t3, t4, t5, t6, t7, t8))))
  def check8NoShrink[T1, T2, T3, T4, T5, T6, T7, T8, R](result: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7], a8: Arbitrary[T8], p: Parameters): Prop =
    Prop.forAllNoShrink(a1.arbitrary, a2.arbitrary, a3.arbitrary, a4.arbitrary, a5.arbitrary, a6.arbitrary, a7.arbitrary, a8.arbitrary)((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8) => collectData(t1, t2, t3, t4, t5, t6, t7, t8)(toProp(result(t1, t2, t3, t4, t5, t6, t7, t8))))

  /** execute a PartialFunction as a ScalaCheck property */
  def prop[T, S](f: PartialFunction[T, S])(implicit toProp: S => Prop, a: Arbitrary[T], s: Shrink[T]): Prop = checkPartial(f)
  def propNoShrink[T, S](f: PartialFunction[T, S])(implicit toProp: S => Prop, a: Arbitrary[T]): Prop = checkPartialNoShrink(f)

  implicit def checkPartial[T, S](f: PartialFunction[T, S])(implicit toProp: S => Prop, a: Arbitrary[T], s: Shrink[T]): Prop =
    PartialFunctionPropertyImplicits.partialFunctionToProp(f).forAll
  implicit def checkPartialNoShrink[T, S](f: PartialFunction[T, S])(implicit toProp: S => Prop, a: Arbitrary[T]): Prop =
    PartialFunctionPropertyImplicits.partialFunctionToProp(f).forAllNoShrink
}
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

  /** @return a Prop that will not throw an exception when evaluated */
  implicit def propToProp(p: =>Prop): Prop = new Prop {
    def apply(params: Gen.Parameters) = {
      try p(params)
      catch {
        case execute.FailureException(f) if f.details != execute.NoDetails =>
          (Prop.falsified :| (f.message+" ("+f.location+")"))(params).collect(f.details)

        case execute.FailureException(f) =>
          (Prop.falsified :| (f.message+" ("+f.location+")"))(params)

        case e: Throwable => Prop.exception(e)(params)
      }
    }
  }

  implicit def booleanToProp(b: =>Boolean): Prop = resultProp(if (b) execute.Success() else execute.Failure())
  implicit def callByNameMatchResultToProp[T](m: =>MatchResult[T]): Prop = resultProp(m.toResult)
  implicit def matchResultToProp[T](m: MatchResult[T]): Prop = resultProp(m.toResult)

  implicit def resultProp(r: =>execute.Result): Prop = {

    new Prop {
      def apply(params: Gen.Parameters) = {
        lazy val result = execute.ResultExecution.execute(r)
        val prop = 
          result match {
            case f : execute.Failure => Prop.falsified :| (f.message+" ("+f.location+")")
            case s : execute.Skipped => Prop.exception(new execute.SkipException(s))
            case p : execute.Pending => Prop.exception(new execute.PendingException(p))
            case e : execute.Error   => Prop.exception(e.exception)
            case other               => Prop.passed
          }

        result match {
          case f: execute.Failure if f.details != execute.NoDetails =>
            prop.apply(params).collect(f.details)

          case _ =>
            prop.apply(params)
        }
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
    def apply[R](f: T => R)(implicit toProp: (=>R) => Prop, s: Shrink[T], params: Parameters) = prop(f)(toProp, a, s, params)
  }
  implicit def applicableArbitrary2[T1, T2](a: (Arbitrary[T1], Arbitrary[T2])) = ApplicableArbitrary2(a._1, a._2)
  case class ApplicableArbitrary2[T1, T2](a1: Arbitrary[T1], a2: Arbitrary[T2]) {
    def apply[R](f: (T1, T2) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], p: Parameters) = prop(f)(toProp, a1, s1, a2, s2, p)
  }
  implicit def applicableArbitrary3[T1, T2, T3](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3])) = ApplicableArbitrary3(a._1, a._2, a._3)
  case class ApplicableArbitrary3[T1, T2, T3](a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3]) {
    def apply[R](f: (T1, T2, T3) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], p: Parameters) = prop(f)(toProp, a1, s1, a2, s2, a3, s3, p)
  }
  implicit def applicableArbitrary4[T1, T2, T3, T4](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3], Arbitrary[T4])) = ApplicableArbitrary4(a._1, a._2, a._3, a._4)
  case class ApplicableArbitrary4[T1, T2, T3, T4](a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4]) {
    def apply[R](f: (T1, T2, T3, T4) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4], p: Parameters) = prop(f)(toProp, a1, s1, a2, s2, a3, s3, a4, s4, p)
  }
  implicit def applicableArbitrary5[T1, T2, T3, T4, T5](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3], Arbitrary[T4], Arbitrary[T5])) = ApplicableArbitrary5(a._1, a._2, a._3, a._4, a._5)
  case class ApplicableArbitrary5[T1, T2, T3, T4, T5](a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5]) {
    def apply[R](f: (T1, T2, T3, T4, T5) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4], s5: Shrink[T5], p: Parameters) = prop(f)(toProp, a1, s1, a2, s2, a3, s3, a4, s4, a5, s5, p)
  }
  implicit def applicableArbitrary6[T1, T2, T3, T4, T5, T6](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3], Arbitrary[T4], Arbitrary[T5], Arbitrary[T6])) = ApplicableArbitrary6(a._1, a._2, a._3, a._4, a._5, a._6)
  case class ApplicableArbitrary6[T1, T2, T3, T4, T5, T6](a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6]) {
    def apply[R](f: (T1, T2, T3, T4, T5, T6) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4], s5: Shrink[T5], s6: Shrink[T6], p: Parameters) = prop(f)(toProp, a1, s1, a2, s2, a3, s3, a4, s4, a5, s5, a6, s6, p)
  }
  implicit def applicableArbitrary7[T1, T2, T3, T4, T5, T6, T7](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3], Arbitrary[T4], Arbitrary[T5], Arbitrary[T6], Arbitrary[T7])) = ApplicableArbitrary7(a._1, a._2, a._3, a._4, a._5, a._6, a._7)
  case class ApplicableArbitrary7[T1, T2, T3, T4, T5, T6, T7](a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7]) {
    def apply[R](f: (T1, T2, T3, T4, T5, T6, T7) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4], s5: Shrink[T5], s6: Shrink[T6], s7: Shrink[T7], p: Parameters) = prop(f)(toProp, a1, s1, a2, s2, a3, s3, a4, s4, a5, s5, a6, s6, a7, s7, p)
  }
  implicit def applicableArbitrary8[T1, T2, T3, T4, T5, T6, T7, T8](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3], Arbitrary[T4], Arbitrary[T5], Arbitrary[T6], Arbitrary[T7], Arbitrary[T8])) = ApplicableArbitrary8(a._1, a._2, a._3, a._4, a._5, a._6, a._7, a._8)
  case class ApplicableArbitrary8[T1, T2, T3, T4, T5, T6, T7, T8](a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7], a8: Arbitrary[T8]) {
    def apply[R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4], s5: Shrink[T5], s6: Shrink[T6], s7: Shrink[T7], s8: Shrink[T8], p: Parameters) = prop(f)(toProp, a1, s1, a2, s2, a3, s3, a4, s4, a5, s5, a6, s6, a7, s7, a8, s8, p)
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

  /** set specific execution parameters on a Property */
  implicit def setProperty(p: Prop)(implicit params: Parameters, prettyParams: Pretty.Params) = new SetProperty(p)(params, prettyParams)
  class SetProperty(prop: Prop)(implicit params: Parameters, prettyParams: Pretty.Params) {
    /** create parameters with verbose = false */
    def set(minTestsOk: Int                              = params.minTestsOk,
            minSize: Int                                 = params.minSize,
            maxDiscardRatio: Float                       = params.maxDiscardRatio,
            maxSize: Int                                 = params.maxSize,
            workers: Int                                 = params.workers,
            rng: scala.util.Random                       = params.rng,
            callback: Test.TestCallback                  = params.callback,
            loader: Option[ClassLoader]                  = params.loader,
            output: Output                               = params.output): execute.Result = {
      check(prop)(new Parameters(minTestsOk, minSize, maxDiscardRatio, maxSize, workers, rng, callback, loader, verbose = false, collect = false, params.prettyCollected, output))
    }
  }

  /** set specific execution parameters on a Property */
  implicit def displayProperty(p: Prop)(implicit params: Parameters, prettyParams: Pretty.Params) = new DisplayProperty(p)(params, prettyParams)
  class DisplayProperty(prop: Prop)(implicit params: Parameters, prettyParams: Pretty.Params) {
    /** create parameters with verbose = true */
    def display(minTestsOk: Int             = params.minTestsOk,
                minSize: Int                = params.minSize,
                maxDiscardRatio: Float      = params.maxDiscardRatio,
                maxSize: Int                = params.maxSize,
                workers: Int                = params.workers,
                rng: scala.util.Random      = params.rng,
                callback: Test.TestCallback = params.callback,
                loader: Option[ClassLoader] = params.loader,
                output: Output              = outer): execute.Result = {
      check(prop)(new Parameters(minTestsOk, minSize, maxDiscardRatio, maxSize, workers, rng, callback, loader, verbose = true, collect = false, params.prettyCollected, output))
    }
  }

  /** create parameters with verbose = false */
  def set(minTestsOk: Int                              = defaultParameters.minTestsOk,
          minSize: Int                                 = defaultParameters.minSize,
          maxDiscardRatio: Float                       = defaultParameters.maxDiscardRatio,
          maxSize: Int                                 = defaultParameters.maxSize,
          workers: Int                                 = defaultParameters.workers,
          rng: scala.util.Random                       = defaultParameters.rng,
          callback: Test.TestCallback                  = defaultParameters.callback,
          loader: Option[ClassLoader]                  = defaultParameters.loader,
          collect: Boolean                             = defaultParameters.collect,
          prettyCollected: Prettier[FreqMap[Set[Any]]] = defaultParameters.prettyCollected,
          output: Output                               = outer): Parameters = {
    new Parameters(minTestsOk, minSize, maxDiscardRatio, maxSize, workers, rng, callback, loader, verbose = false, collect, prettyCollected, output)
  }

  /** create parameters with verbose = true */
  def display(minTestsOk: Int                              = defaultParameters.minTestsOk,
              minSize: Int                                 = defaultParameters.minSize,
              maxDiscardRatio: Float                       = defaultParameters.maxDiscardRatio,
              maxSize: Int                                 = defaultParameters.maxSize,
              workers: Int                                 = defaultParameters.workers,
              rng: scala.util.Random                       = defaultParameters.rng,
              callback: Test.TestCallback                  = defaultParameters.callback,
              loader: Option[ClassLoader]                  = defaultParameters.loader,
              collect: Boolean                             = defaultParameters.collect,
              prettyCollected: Prettier[FreqMap[Set[Any]]] = defaultParameters.prettyCollected,
              output: Output                               = outer): Parameters = {
    new Parameters(minTestsOk, minSize, maxDiscardRatio, maxSize, workers, rng, callback, loader, verbose = true, collect, prettyCollected, output)
  }

  /** shortcut to collect and display values */
  def collectValues = display(collect = true)

  /** shortcut to collect, display values and tweak the values */
  def collectValuesAnd(showValues: Set[Any] => Set[Any]) =
    collectValues.prettyCollected(showValues)

}

/**
* This class is the base class for the display and set case classes.<br>
* It contains a Map of generation parameters and indicates if the generation
* must be verbose.
*/
case class Parameters(minTestsOk: Int                              = Test.Parameters.default.minSuccessfulTests,
                      minSize: Int                                 = Test.Parameters.default.minSize,
                      maxDiscardRatio: Float                       = Test.Parameters.default.maxDiscardRatio,
                      maxSize: Int                                 = Test.Parameters.default.maxSize,
                      workers: Int                                 = Test.Parameters.default.workers,
                      rng: scala.util.Random                       = Test.Parameters.default.rng,
                      callback: Test.TestCallback                  = Test.Parameters.default.testCallback,
                      loader: Option[ClassLoader]                  = Test.Parameters.default.customClassLoader,
                      verbose: Boolean                             = false,
                      collect: Boolean                             = false,
                      prettyCollected: Prettier[FreqMap[Set[Any]]] = Prettier.defaultPrettierMap,
                      output: Output                               = Output.NoOutput) { outer =>

  def testCallback(implicit pretty: Pretty.Params) = if (verbose) callback.chain(verboseCallback) else callback

  def toScalaCheckParameters(implicit pretty: Pretty.Params): Test.Parameters =
    new Test.Parameters {
      val minSuccessfulTests = outer.minTestsOk
      val maxDiscardRatio    = outer.maxDiscardRatio
      val maxSize            = outer.maxSize
      val minSize            = outer.minSize
      val workers            = outer.workers
      val rng                = outer.rng
      val testCallback       = outer.testCallback
      val customClassLoader  = outer.loader
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

  def prettyCollected(showValues: Set[Any] => Set[Any]) =
    copy(prettyCollected = Prettier.prettierValues(showValues))
}

/**
 * Type-class for objects having a Pretty instance
 */
trait Prettier[T] {
  def pretty(t: =>T): Pretty
}

object Prettier {

  /** Prettier instance for FreqMaps */
  def createPrettierFreqMap(show: FreqMap[Set[Any]] => Pretty.Params => String) = new Prettier[FreqMap[Set[Any]]] {
    def pretty(freqMap: =>FreqMap[Set[Any]]) = Pretty(show(freqMap))
  }

  /** Prettier instance for FreqMaps */
  def prettierFreqMap(show: FreqMap[Set[Any]] => String) =
    createPrettierFreqMap((freqMap: FreqMap[Set[Any]]) => (prms: Pretty.Params) => show(freqMap))

  /** Default prettier instance for FreqMaps */
  def defaultPrettierMap: Prettier[FreqMap[Set[Any]]] =
    createPrettierFreqMap((freqMap: FreqMap[Set[Any]]) => (prms: Pretty.Params) => Pretty.prettyFreqMap(freqMap)(prms))

  /** Prettier instance for FreqMaps with a custom function to get a better display */
  def prettierValues(showValues: Set[Any] => Set[Any]) =
    prettierFreqMap { fm: FreqMap[Set[Any]] =>
      if (fm.total == 0) ""
      else {
        s"> Collected test data" / {
          for {
            (xs,r) <- fm.getRatios
            ys = xs - (())
            if !ys.isEmpty
          } yield round(r*100)+"% " + showValues(ys).mkString(", ")
        }.mkString("\n")
      }
    }
 }

/**
 * This trait can be mixed in a Specification to avoid counting the number of times that a property was executed as the
 * number of expectations. With this trait we just count 1 for each result
 */
trait OneExpectationPerProp extends ScalaCheckMatchers {
  private def superPropAsResult[P <: Prop] = super.propAsResult[P]

  override implicit def propAsResult[P <: Prop](implicit p: Parameters): AsResult[P] = new AsResult[P] {
    def asResult(prop: =>P) = superPropAsResult.asResult(prop).setExpectationsNb(1)
  }
}
