package org.specs2
package matcher
import org.scalacheck.{ Gen, Prop, Arg, Test, Arbitrary, Shrink }
import org.scalacheck.util.StdRand
import org.scalacheck.Prop._
import org.scalacheck.Test.{ Status, Params, Proved, Passed, Failed, Exhausted, GenException, PropException, Result }
import org.scalacheck.Pretty._
import org.scalacheck.Pretty
import org.scalacheck.ConsoleReporter._
import scala.collection.Map
import io.ConsoleOutput
import text.Plural._
/**
 * The <code>ScalaCheckMatchers</code> trait provides matchers which allow to
 * assess properties multiple times with generated data.
 * @see the <a href="http://code.google.com/p/scalacheck/">ScalaCheck project</a>
 */
trait ScalaCheck extends ConsoleOutput with ScalaCheckFunctions with ScalaCheckParameters with PropertyImplicits { outer =>
  /** execute a PartialFunction as a ScalaCheck property */
  implicit def checkPartial[T, S](f: PartialFunction[T, Boolean])(implicit a: Arbitrary[T], s: Shrink[T], p: Parameters): execute.Result = {
	checkProp(f.forAll(a, s))(p)
  }
  /** execute a Function as a ScalaCheck property */
  implicit def checkFunction[T, S](f: T => Boolean)(implicit a: Arbitrary[T], s: Shrink[T], p: Parameters): execute.Result = {
	checkProp(f.forAll(a, s))(p)
  }
  /** 
   * execute a Function returning a MatchResult as a ScalaCheck property
   * This implicit is only added because the most powerful type inference of the check method 
   * defined hereafter doesn't work and check has to be used explicitly in the case
   * where the input type of the function is different from the MatchResult type
   */
  implicit def checkResult[T](result: T => MatchResult[T])(implicit a: Arbitrary[T], s: Shrink[T], p: Parameters): execute.Result = {
	result.forAll(a, s)
  }
  /** 
   * execute a Function returning a MatchResult as a ScalaCheck property
   * this must be used when the input type of the function is different from the MatchResult type 
   */
  implicit def check[T, S](result: T => MatchResult[S])(implicit a: Arbitrary[T], s: Shrink[T], p: Parameters): execute.Result = {
	result.forAll(a, s)
  }
  /** execute a ScalaCheck property */
  implicit def checkProp(prop: Prop)(implicit p: Parameters): execute.Result = checkProperty(prop)(p)
  /**
   * checks if the property is true for each generated value, and with the specified
   * generation parameters <code>p</code>. <code>p</code> is transformed into a scalacheck parameters
   * and indicates if the generation should be verbose or not
   */
  private[matcher] def checkProperty(prop: =>Prop)(implicit p: Parameters): execute.Result = {
    checkScalaCheckProperty(prop)(Params(p(minTestsOk), p(maxDiscarded), p(minSize), p(maxSize), StdRand, p(workers), p(wrkSize)), p.verbose)
  }

  /**
   * checks if the property is true for each generated value, and with the specified
   * scalacheck parameters. If verbose is true, then print the results on the console
   */
  private [matcher] def checkScalaCheckProperty(prop: =>Prop)(params: Params, verbose: Boolean): execute.Result = {
    // will print the result of each test if verbose = true
    def printResult(succeeded: Int, discarded: Int): Unit = {
      if (!verbose) return
      if (discarded == 0)
        printf("\rPassed %d tests", succeeded)
      else
        printf("\rPassed %d tests; %d discarded", succeeded, discarded)
      flush
    }

    // check the property with ScalaCheck
    val results = checkProp(params, prop, printResult)

    // display the final result if verbose = true
    if (verbose) {
      val s = prettyTestRes(results)(defaultPrettyParams)
      printf("\r%s %s%s\n", if (results.passed) "+" else "!", s, List.fill(70 - s.length)(" ").mkString(""))
    }
    results match {
      case Result(Proved(as), succeeded, discarded, _) => 
        execute.Success(noCounterExample(succeeded), succeeded)
      case Result(Passed, succeeded, discarded, _) => 
        execute.Success(noCounterExample(succeeded), succeeded)
      case r @ Result(GenException(e), n, _, _) => 
        execute.Failure(prettyTestRes(r)(defaultPrettyParams))
      case r @ Result(Exhausted, n, _, _) => 
        execute.Failure(prettyTestRes(r)(defaultPrettyParams))
      case Result(Failed(args, labels), n, _, _) =>
        execute.Failure("A counter-example is "+counterExample(args)+" (" + afterNTries(n) + afterNShrinks(args) + ")" + failedLabels(labels))
      case Result(PropException(args, ex, labels), n, _, _) =>
        execute.Error("A counter-example is "+counterExample(args)+": " + ex + " ("+afterNTries(n)+")"+ failedLabels(labels))
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
    else labels.mkString("\nlabels of failing property: ", ", ", "\n")
  }
}
/**
 * This trait adds some syntactic sugar to transform function
 * to properties by appending forAll
 */
trait PropertyImplicits {
  /** transform a function returning a boolean to a property by appending forAll */
  implicit def functionToProp[T](f: T => Boolean): FunctionForAll[T] = new FunctionForAll(f)
  class FunctionForAll[T](f: T => Boolean) {
    def forAll(implicit a: Arbitrary[T], s: Shrink[T]): Prop = Prop.forAll(f)
  }
  /** transform a partial function returning a boolean to a property by appending forAll */
  implicit def partialFunctionToProp[T](f: PartialFunction[T, Boolean]): PartialFunctionForAll[T] = new PartialFunctionForAll(f)
  class PartialFunctionForAll[T](f: PartialFunction[T, Boolean]) {
    def forAll(implicit a: Arbitrary[T], s: Shrink[T]): Prop = Prop.forAll(f)
  }
  /** transform a function returning a MatchResult to a property by appending forAll */
  implicit def toProp[T](f: T => MatchResult[_]): ForAll2[T] = new ForAll2(f)
  class ForAll2[T](f: T => MatchResult[_]) {
    def forAll(implicit a: Arbitrary[T], s: Shrink[T]): Prop = asProperty(f)
  }
  /** transform a function returning a MatchResult to a property */
  private def asProperty[T](f: T => MatchResult[_])(implicit a: Arbitrary[T], s: Shrink[T]): Prop = {
	Prop.forAll { (t: T) =>
	  f(t) match {
	 	case MatchSuccess(_, _ , _) => true  
	 	case MatchFailure(_, _ , _) => false  
	 	case MatchSkip(_, _) => true  
	  } 	
	}
  }

}
/**
 * This trait is used to facilitate testing by mocking ScalaCheck functionalities
 */
trait ScalaCheckFunctions {
  def checkProp(params: Params, prop: =>Prop, printResult: (Int, Int) => Unit) = Test.check(params, prop, printResult)
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
  val (minSize, maxSize, maxDiscarded, minTestsOk, workers, wrkSize) = ('minSize, 'maxSize, 'maxDiscarded, 'minTestsOk, 'workers, 'wrkSize)
   /**
    * default parameters. Uses ScalaCheck default values and doesn't print anything to the console
    */
   implicit def defaultParameters = new Parameters(setParams(Nil))

   /** default parameters to display pretty messages */		   
   val defaultPrettyParams = Pretty.defaultParams
   /**
    * Default values for ScalaCheck parameters
    */
   def defaultValues = Map(minTestsOk->100, maxDiscarded ->500, minSize->0, maxSize->100, workers->1, wrkSize->20)

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
