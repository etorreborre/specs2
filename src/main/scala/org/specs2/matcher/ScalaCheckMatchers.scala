package org.specs2
package matcher

import org.scalacheck.util.StdRand
import org.scalacheck.Prop._
import org.scalacheck.Test.{ Params, Proved, Passed, Failed, Exhausted, GenException, PropException, Result }
import org.scalacheck.Pretty._
import scala.collection.Map
import io.ConsoleOutput
import org.scalacheck._
import execute.AsResult


/**
 * The ScalaCheckMatchers trait provides matchers which allow to
 * assess properties multiple times with generated data.
 * @see the <a href="http://code.google.com/p/scalacheck/">ScalaCheck project</a>
 */
trait ScalaCheckMatchers extends ConsoleOutput with ScalaCheckFunctions with ScalaCheckParameters
   with FunctionPropertyImplicits
   with ResultPropertyImplicits
   with ApplicableArbitraries
   with Expectations { outer: ScalaCheckMatchers =>

  /** implicit typeclass instance to create examples from Props */
  implicit def propAsResult(implicit p: Parameters): AsResult[Prop] = new AsResult[Prop] {
    def asResult(prop: =>Prop): execute.Result = checkProp(prop)(p)
  }
  /**
   * transform a Function returning a MatchResult (or anything which can be converted to a Prop) as a ScalaCheck property
   */
  def prop[T, R](result: T => R)(implicit toProp: (=>R) => Prop, a: Arbitrary[T], s: Shrink[T]): Prop = check1(result)
  /**
   * @deprecated use prop instead. The name "check" is misleading because it doesn't actually check anything but merely builds a Prop
   */
  def check[T, R](result: T => R)(implicit toProp: (=>R) => Prop, a: Arbitrary[T], s: Shrink[T]): Prop = check1(result)
  implicit def check1[T, R](result: T => R)(implicit toProp: (=>R) => Prop, a: Arbitrary[T], s: Shrink[T]): Prop = Prop.forAll((t: T) => toProp(result(t)))

  def prop[T1, T2, R](result: (T1, T2) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2]): Prop = check2(result)
  /** @deprecated use forAll instead */
  def check[T1, T2, R](result: (T1, T2) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2]): Prop = check2(result)
  implicit def check2[T1, T2, R](result: (T1, T2) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2]): Prop =
    Prop.forAll((t1: T1, t2: T2) => toProp(result(t1, t2)))

  def prop[T1, T2, T3, R](result: (T1, T2, T3) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3]): Prop = check3(result)
  /** @deprecated use prop instead */
  def check[T1, T2, T3, R](result: (T1, T2, T3) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3]): Prop = check3(result)
  implicit def check3[T1, T2, T3, R](result: (T1, T2, T3) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3]): Prop =
    Prop.forAll((t1: T1, t2: T2, t3: T3) => toProp(result(t1, t2, t3)))

  def prop[T1, T2, T3, T4, R](result: (T1, T2, T3, T4) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4]): Prop = check4(result)
  /** @deprecated use prop instead */
  def check[T1, T2, T3, T4, R](result: (T1, T2, T3, T4) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4]): Prop = check4(result)
  implicit def check4[T1, T2, T3, T4, R](result: (T1, T2, T3, T4) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4]): Prop =
    Prop.forAll((t1: T1, t2: T2, t3: T3, t4: T4) => toProp(result(t1, t2, t3, t4)))

  def prop[T1, T2, T3, T4, T5, R](result: (T1, T2, T3, T4, T5) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5]): Prop = check5(result)
  /** @deprecated use prop instead */
  def check[T1, T2, T3, T4, T5, R](result: (T1, T2, T3, T4, T5) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5]): Prop = check5(result)
  implicit def check5[T1, T2, T3, T4, T5, R](result: (T1, T2, T3, T4, T5) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5]): Prop =
    Prop.forAll((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5) => toProp(result(t1, t2, t3, t4, t5)))

  def prop[T1, T2, T3, T4, T5, T6, R](result: (T1, T2, T3, T4, T5, T6) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1],a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5], a6: Arbitrary[T6], s6: Shrink[T6]): Prop = check6(result)
  /** @deprecated use prop instead */
  def check[T1, T2, T3, T4, T5, T6, R](result: (T1, T2, T3, T4, T5, T6) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1],a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5], a6: Arbitrary[T6], s6: Shrink[T6]): Prop = check6(result)
  implicit def check6[T1, T2, T3, T4, T5, T6, R](result: (T1, T2, T3, T4, T5, T6) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1],a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5], a6: Arbitrary[T6], s6: Shrink[T6]): Prop =
    Prop.forAll((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6) => toProp(result(t1, t2, t3, t4, t5, t6)))

  def prop[T1, T2, T3, T4, T5, T6, T7, R](result: (T1, T2, T3, T4, T5, T6, T7) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2],a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5], a6: Arbitrary[T6], s6: Shrink[T6], a7: Arbitrary[T7], s7: Shrink[T7]): Prop = check7(result)
  /** @deprecated use prop instead */
  def check[T1, T2, T3, T4, T5, T6, T7, R](result: (T1, T2, T3, T4, T5, T6, T7) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2],a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5], a6: Arbitrary[T6], s6: Shrink[T6], a7: Arbitrary[T7], s7: Shrink[T7]): Prop = check7(result)
  implicit def check7[T1, T2, T3, T4, T5, T6, T7, R](result: (T1, T2, T3, T4, T5, T6, T7) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2],a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5], a6: Arbitrary[T6], s6: Shrink[T6], a7: Arbitrary[T7], s7: Shrink[T7]): Prop =
    Prop.forAll((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7) => toProp(result(t1, t2, t3, t4, t5, t6, t7)))

  def prop[T1, T2, T3, T4, T5, T6, T7, T8, R](result: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5], a6: Arbitrary[T6], s6: Shrink[T6], a7: Arbitrary[T7], s7: Shrink[T7], a8: Arbitrary[T8], s8: Shrink[T8]): Prop = check8(result)
  /** @deprecated use prop instead */
  def check[T1, T2, T3, T4, T5, T6, T7, T8, R](result: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5], a6: Arbitrary[T6], s6: Shrink[T6], a7: Arbitrary[T7], s7: Shrink[T7], a8: Arbitrary[T8], s8: Shrink[T8]): Prop = check8(result)
  implicit def check8[T1, T2, T3, T4, T5, T6, T7, T8, R](result: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(implicit toProp: (=>R) => Prop, a1: Arbitrary[T1], s1: Shrink[T1], a2: Arbitrary[T2], s2: Shrink[T2], a3: Arbitrary[T3], s3: Shrink[T3], a4: Arbitrary[T4], s4: Shrink[T4], a5: Arbitrary[T5], s5: Shrink[T5], a6: Arbitrary[T6], s6: Shrink[T6], a7: Arbitrary[T7], s7: Shrink[T7], a8: Arbitrary[T8], s8: Shrink[T8]): Prop =
    Prop.forAll((t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8) => toProp(result(t1, t2, t3, t4, t5, t6, t7, t8)))
  
  /** execute a PartialFunction as a ScalaCheck property */
  def prop[T, S](f: PartialFunction[T, S])(implicit toProp: S => Prop, a: Arbitrary[T], s: Shrink[T]): Prop = checkPartial(f)
  /** @deprecated use prop instead */
  def check[T, S](f: PartialFunction[T, S])(implicit toProp: S => Prop, a: Arbitrary[T], s: Shrink[T]): Prop = checkPartial(f)
  implicit def checkPartial[T, S](f: PartialFunction[T, S])(implicit toProp: S => Prop, a: Arbitrary[T], s: Shrink[T]): Prop =
    PartialFunctionPropertyImplicits.partialFunctionToProp(f).forAll

  /** execute a ScalaCheck property */
  def check(prop: Prop)(implicit p: Parameters): execute.Result = checkProp(prop)(p)

  /** execute a ScalaCheck property */
  implicit def checkProp(prop: Prop)(implicit p: Parameters): execute.Result =
    checkResultFailure(checkProperty(prop)(p))

  /** set specific execution parameters on a Property */
  implicit def setProperty(p: Prop) = new SetProperty(p)
  class SetProperty(prop: Prop) {
    def set(p: (Symbol, Int)*) = check(prop)(outer.set(p:_*))
    def display(p: (Symbol, Int)*) = check(prop)(outer.display(p:_*))
  }
  
  /**
   * checks if the property is true for each generated value, and with the specified
   * generation parameters <code>p</code>. <code>p</code> is transformed into a scalacheck parameters
   * and indicates if the generation should be verbose or not
   */
  private[specs2] def checkProperty(prop: Prop)(implicit p: Parameters): execute.Result = {
    checkScalaCheckProperty(prop)(Params(p(minTestsOk), p(maxDiscarded), p(minSize), p(maxSize), StdRand, p(workers)), p.verbose)
  }

  /**
   * checks if the property is true for each generated value, and with the specified
   * scalacheck parameters. If verbose is true, then print the results on the console
   */
  private [matcher] def checkScalaCheckProperty(prop: =>Prop)(params: Params, verbose: Boolean): execute.Result = {
    // will print the result of each test if verbose = true
    val callback = new Test.TestCallback {
      override def onPropEval(name: String, threadXdx: Int, succeeded: Int, discarded: Int): Unit = {
        if (verbose) {
          if (discarded == 0)
            printf("\rPassed %d tests", succeeded)
          else
            printf("\rPassed %d tests; %d discarded", succeeded, discarded)
          flush
        }
      }
    }

    // check the property with ScalaCheck
    val results = checkProp(params, prop, callback)

    // display the final result if verbose = true
    if (verbose) {
      val s = prettyTestRes(results)(defaultPrettyParams)
      printf("\r%s %s%s\n", if (results.passed) "+" else "!", s, List.fill(70 - s.length)(" ").mkString(""))
    }

    def counterExampleMessage(args: Prop.Args, n: Int, labels: Set[String]) =
      "A counter-example is "+counterExample(args)+" (" + afterNTries(n) + afterNShrinks(args) + ")" + failedLabels(labels)

    results match {
      case Result(Proved(as), succeeded, discarded, fq, _) =>
        execute.Success(noCounterExample(succeeded), frequencies(fq), succeeded)
      case Result(Passed, succeeded, discarded, fq, _)     =>
        execute.Success(noCounterExample(succeeded), frequencies(fq), succeeded)
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
  }
}
object PartialFunctionPropertyImplicits extends PartialFunctionPropertyImplicits

trait ResultPropertyImplicits {

  implicit def unitToProp(u: =>Unit): Prop = booleanToProp({u; true})
  implicit def propToProp(p: =>Prop): Prop = p
  implicit def booleanToProp(b: =>Boolean): Prop = resultProp(if (b) execute.Success() else execute.Failure())
  implicit def callByNameMatchResultToProp[T](m: =>MatchResult[T]): Prop = resultProp(m.toResult)
  implicit def matchResultToProp[T](m: MatchResult[T]): Prop = resultProp(m.toResult)

  private def resultProp(r: =>execute.Result): Prop = {
    new Prop {
      def apply(params: Prop.Params) = {
        lazy val result = execute.ResultExecution.execute(r)
        val prop = 
        result match {
          case f : execute.Failure => Prop.falsified              :| (f.message+" ("+f.location+")")
          case s : execute.Skipped => Prop.undecided              :| s.message
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
    def apply[R](f: T => R)(implicit toProp: (=>R) => Prop, s: Shrink[T]) = check(f)(toProp, a, s)
  }
  implicit def applicableArbitrary2[T1, T2](a: (Arbitrary[T1], Arbitrary[T2])) = ApplicableArbitrary2(a._1, a._2)
  case class ApplicableArbitrary2[T1, T2](a1: Arbitrary[T1], a2: Arbitrary[T2]) {
    def apply[R](f: (T1, T2) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2]) = check(f)(toProp, a1, s1, a2, s2)
  }
  implicit def applicableArbitrary3[T1, T2, T3](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3])) = ApplicableArbitrary3(a._1, a._2, a._3)
  case class ApplicableArbitrary3[T1, T2, T3](a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3]) {
    def apply[R](f: (T1, T2, T3) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3]) = check(f)(toProp, a1, s1, a2, s2, a3, s3)
  }
  implicit def applicableArbitrary4[T1, T2, T3, T4](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3], Arbitrary[T4])) = ApplicableArbitrary4(a._1, a._2, a._3, a._4)
  case class ApplicableArbitrary4[T1, T2, T3, T4](a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4]) {
    def apply[R](f: (T1, T2, T3, T4) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4]) = check(f)(toProp, a1, s1, a2, s2, a3, s3, a4, s4)
  }
  implicit def applicableArbitrary5[T1, T2, T3, T4, T5](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3], Arbitrary[T4], Arbitrary[T5])) = ApplicableArbitrary5(a._1, a._2, a._3, a._4, a._5)
  case class ApplicableArbitrary5[T1, T2, T3, T4, T5](a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5]) {
    def apply[R](f: (T1, T2, T3, T4, T5) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4], s5: Shrink[T5]) = check(f)(toProp, a1, s1, a2, s2, a3, s3, a4, s4, a5, s5)
  }
  implicit def applicableArbitrary6[T1, T2, T3, T4, T5, T6](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3], Arbitrary[T4], Arbitrary[T5], Arbitrary[T6])) = ApplicableArbitrary6(a._1, a._2, a._3, a._4, a._5, a._6)
  case class ApplicableArbitrary6[T1, T2, T3, T4, T5, T6](a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6]) {
    def apply[R](f: (T1, T2, T3, T4, T5, T6) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4], s5: Shrink[T5], s6: Shrink[T6]) = check(f)(toProp, a1, s1, a2, s2, a3, s3, a4, s4, a5, s5, a6, s6)
  }
  implicit def applicableArbitrary7[T1, T2, T3, T4, T5, T6, T7](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3], Arbitrary[T4], Arbitrary[T5], Arbitrary[T6], Arbitrary[T7])) = ApplicableArbitrary7(a._1, a._2, a._3, a._4, a._5, a._6, a._7)
  case class ApplicableArbitrary7[T1, T2, T3, T4, T5, T6, T7](a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7]) {
    def apply[R](f: (T1, T2, T3, T4, T5, T6, T7) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4], s5: Shrink[T5], s6: Shrink[T6], s7: Shrink[T7]) = check(f)(toProp, a1, s1, a2, s2, a3, s3, a4, s4, a5, s5, a6, s6, a7, s7)
  }
  implicit def applicableArbitrary8[T1, T2, T3, T4, T5, T6, T7, T8](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3], Arbitrary[T4], Arbitrary[T5], Arbitrary[T6], Arbitrary[T7], Arbitrary[T8])) = ApplicableArbitrary8(a._1, a._2, a._3, a._4, a._5, a._6, a._7, a._8)
  case class ApplicableArbitrary8[T1, T2, T3, T4, T5, T6, T7, T8](a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7], a8: Arbitrary[T8]) {
    def apply[R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4], s5: Shrink[T5], s6: Shrink[T6], s7: Shrink[T7], s8: Shrink[T8]) = check(f)(toProp, a1, s1, a2, s2, a3, s3, a4, s4, a5, s5, a6, s6, a7, s7, a8, s8)
  }

}
/**
 * This trait is used to facilitate testing by mocking ScalaCheck functionalities
 */
trait ScalaCheckFunctions {
  def checkProp(params: Params, prop: =>Prop, callback: Test.TestCallback) = Test.check(params.copy(testCallback = callback), prop)
}
/**
 * This trait provides generation parameters to use with the <code>ScalaCheckMatchers</code>
 */
trait ScalaCheckParameters {
  /**
   * Values which can be used as Symbol aliases to specify ScalaCheck parameters<br>
   * The naming is a bit different, in order to keep short names for frequent use cases<ul>
   *  <code><li>minTestsOk == minSuccessfulTests
   *  <li>maxDiscarded == maxDiscardedTests
   *  <li>minSize and maxSize keep their name <code><ul>
   */
  val (minSize, maxSize, maxDiscarded, minTestsOk, workers) = ('minSize, 'maxSize, 'maxDiscarded, 'minTestsOk, 'workers)
   /**
    * default parameters. Uses ScalaCheck default values and doesn't print anything to the console
    */
   implicit def defaultParameters = new Parameters(setParams(Nil))

   /** default parameters to display pretty messages */		   
   implicit def defaultPrettyParams = Pretty.defaultParams
   /**
    * Default values for ScalaCheck parameters
    */
   def defaultValues = Map(minTestsOk->100, maxDiscarded ->500, minSize->0, maxSize->100, workers->1)

   /** factory object to create parameters with verbose = false */
   object set extends Parameters(setParams(Nil)) {
     def apply(p: (Symbol, Int)*) = new Parameters(setParams(p))
   }
   /** factory object to create parameters with verbose = true */
   object display  extends Parameters(setParams(Nil)) {
     def apply(p: (Symbol, Int)*) = new Parameters(setParams(p)) { override def verbose = true }
     override def verbose = true
   }
   private def setParams(p: Seq[(Symbol, Int)]): Map[Symbol, Int] = {
     p.foldLeft(defaultValues) { (res: Map[Symbol, Int], pair: (Symbol, Int)) =>
       //  this is a useful check in case of print(null) or set(null)
       if (pair == null || pair._1 == null)
         throw new RuntimeException("null values are not accepted in scalacheck parameters: '"+pair+"'")
       res updated (pair._1, pair._2)
     }
  }
}
/**
 * This class is the base class for the display and set case classes.<br>
 * It contains a Map of generation parameters and indicates if the generation
 * must be verbose.
 */
case class Parameters(params: Map[Symbol, Int]) {
  def apply(s: Symbol) = params(s)
  def verbose = false
}
