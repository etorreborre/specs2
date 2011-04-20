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
import text.Plural._
import io.ConsoleOutput


/**
 * The ScalaCheckMatchers trait provides matchers which allow to
 * assess properties multiple times with generated data.
 * @see the <a href="http://code.google.com/p/scalacheck/">ScalaCheck project</a>
 */
trait ScalaCheckMatchers extends ConsoleOutput with ScalaCheckFunctions with ScalaCheckParameters with PropertyImplicits { outer =>
  /** execute a PartialFunction as a ScalaCheck property */
  implicit def checkPartial[T, S](f: PartialFunction[T, Boolean])(implicit a: Arbitrary[T], s: Shrink[T], p: Parameters): execute.Result = {
	  checkProp(f.forAll(a, s))(p)
  }
  /** execute a Function as a ScalaCheck property */
  implicit def checkFunction[T](f: T => Boolean)(implicit a: Arbitrary[T], s: Shrink[T], p: Parameters): execute.Result = {
	  checkProp(f.forAll(a, s))(p)
  }
  /** 
   * execute a Function returning a MatchResult as a ScalaCheck property
   */
  def checkResult[T](result: T => execute.Result)(implicit a: Arbitrary[T], s: Shrink[T], p: Parameters): execute.Result = {
    checkProp(resultAsProperty(result))(p)
  }
  implicit def checkResult1[T](result: T => MatchResult[T])(implicit a: Arbitrary[T], s: Shrink[T], p: Parameters): execute.Result = {
    checkProp(result.forAll(a, s))(p)
  }
  implicit def checkResult2[T1, T2](result: (T1, T2) => execute.Result)
    (implicit
        a1: Arbitrary[T1], s1: Shrink[T1],
        a2: Arbitrary[T2], s2: Shrink[T2],
        p: Parameters): execute.Result = {
     checkProp(resultAsProperty(result))(p)
  }
  def checkResult[T1, T2](result: (T1, T2) => execute.Result)
    (implicit
        a1: Arbitrary[T1], s1: Shrink[T1],
        a2: Arbitrary[T2], s2: Shrink[T2],
        p: Parameters): execute.Result = {
     checkProp(resultAsProperty(result))(p)
  }
  implicit def checkResult3[T1, T2, T3](result: (T1, T2, T3) => execute.Result)
    (implicit
        a1: Arbitrary[T1], s1: Shrink[T1],
        a2: Arbitrary[T2], s2: Shrink[T2],
        a3: Arbitrary[T3], s3: Shrink[T3],
        p: Parameters): execute.Result = {
     checkProp(resultAsProperty(result))(p)
  }
  def checkResult[T1, T2, T3](result: (T1, T2, T3) => execute.Result)
    (implicit
        a1: Arbitrary[T1], s1: Shrink[T1],
        a2: Arbitrary[T2], s2: Shrink[T2],
        a3: Arbitrary[T3], s3: Shrink[T3],
        p: Parameters): execute.Result = {
     checkProp(resultAsProperty(result))(p)
  }
  implicit def checkResult4[T1, T2, T3, T4](result: (T1, T2, T3, T4) => execute.Result)
    (implicit
        a1: Arbitrary[T1], s1: Shrink[T1],
        a2: Arbitrary[T2], s2: Shrink[T2],
        a3: Arbitrary[T3], s3: Shrink[T3],
        a4: Arbitrary[T4], s4: Shrink[T4],
        p: Parameters): execute.Result = {
     checkProp(resultAsProperty(result))(p)
  }
  def checkResult[T1, T2, T3, T4](result: (T1, T2, T3, T4) => execute.Result)
    (implicit
        a1: Arbitrary[T1], s1: Shrink[T1],
        a2: Arbitrary[T2], s2: Shrink[T2],
        a3: Arbitrary[T3], s3: Shrink[T3],
        a4: Arbitrary[T4], s4: Shrink[T4],
        p: Parameters): execute.Result = {
     checkProp(resultAsProperty(result))(p)
  }
  implicit def checkResult5[T1, T2, T3, T4, T5](result: (T1, T2, T3, T4, T5) => execute.Result)
    (implicit
        a1: Arbitrary[T1], s1: Shrink[T1],
        a2: Arbitrary[T2], s2: Shrink[T2],
        a3: Arbitrary[T3], s3: Shrink[T3],
        a4: Arbitrary[T4], s4: Shrink[T4],
        a5: Arbitrary[T5], s5: Shrink[T5],
        p: Parameters): execute.Result = {
     checkProp(resultAsProperty(result))(p)
  }
  def checkResult[T1, T2, T3, T4, T5](result: (T1, T2, T3, T4, T5) => execute.Result)
    (implicit
        a1: Arbitrary[T1], s1: Shrink[T1],
        a2: Arbitrary[T2], s2: Shrink[T2],
        a3: Arbitrary[T3], s3: Shrink[T3],
        a4: Arbitrary[T4], s4: Shrink[T4],
        a5: Arbitrary[T5], s5: Shrink[T5],
        p: Parameters): execute.Result = {
     checkProp(resultAsProperty(result))(p)
  }
  /**
   * execute a Function returning a MatchResult as a ScalaCheck property
   * this must be used when the input type of the function is different from the MatchResult type
   */
  def check[T](result: T => MatchResult[_])(implicit a: Arbitrary[T], s: Shrink[T], p: Parameters): execute.Result = {
    checkProp(result.forAll(a, s))(p)
  }
  implicit def check2[T1, T2](result: (T1, T2) => MatchResult[_])
    (implicit
        a1: Arbitrary[T1], s1: Shrink[T1],
        a2: Arbitrary[T2], s2: Shrink[T2],
        p: Parameters): execute.Result = {
     checkProp(asProperty(result))(p)
  }
  def check[T1, T2](result: (T1, T2) => MatchResult[_])
    (implicit 
        a1: Arbitrary[T1], s1: Shrink[T1], 
        a2: Arbitrary[T2], s2: Shrink[T2], 
        p: Parameters): execute.Result = {
     checkProp(asProperty(result))(p)
  }
  implicit def check3[T1, T2, T3](result: (T1, T2, T3) => MatchResult[_])
    (implicit
        a1: Arbitrary[T1], s1: Shrink[T1],
        a2: Arbitrary[T2], s2: Shrink[T2],
        a3: Arbitrary[T3], s3: Shrink[T3],
        p: Parameters): execute.Result = {
     checkProp(asProperty(result))(p)
  }
  def check[T1, T2, T3](result: (T1, T2, T3) => MatchResult[_])
    (implicit
        a1: Arbitrary[T1], s1: Shrink[T1],
        a2: Arbitrary[T2], s2: Shrink[T2],
        a3: Arbitrary[T3], s3: Shrink[T3],
        p: Parameters): execute.Result = {
     checkProp(asProperty(result))(p)
  }
  implicit def check4[T1, T2, T3, T4](result: (T1, T2, T3, T4) => MatchResult[_])
    (implicit
        a1: Arbitrary[T1], s1: Shrink[T1],
        a2: Arbitrary[T2], s2: Shrink[T2],
        a3: Arbitrary[T3], s3: Shrink[T3],
        a4: Arbitrary[T4], s4: Shrink[T4],
        p: Parameters): execute.Result = {
     checkProp(asProperty(result))(p)
  }
  def check[T1, T2, T3, T4](result: (T1, T2, T3, T4) => MatchResult[_])
    (implicit
        a1: Arbitrary[T1], s1: Shrink[T1],
        a2: Arbitrary[T2], s2: Shrink[T2],
        a3: Arbitrary[T3], s3: Shrink[T3],
        a4: Arbitrary[T4], s4: Shrink[T4],
        p: Parameters): execute.Result = {
     checkProp(asProperty(result))(p)
  }
  implicit def check5[T1, T2, T3, T4, T5](result: (T1, T2, T3, T4, T5) => MatchResult[_])
    (implicit
        a1: Arbitrary[T1], s1: Shrink[T1],
        a2: Arbitrary[T2], s2: Shrink[T2],
        a3: Arbitrary[T3], s3: Shrink[T3],
        a4: Arbitrary[T4], s4: Shrink[T4],
        a5: Arbitrary[T5], s5: Shrink[T5],
        p: Parameters): execute.Result = {
     checkProp(asProperty(result))(p)
  }
  def check[T1, T2, T3, T4, T5](result: (T1, T2, T3, T4, T5) => MatchResult[_])
    (implicit
        a1: Arbitrary[T1], s1: Shrink[T1],
        a2: Arbitrary[T2], s2: Shrink[T2],
        a3: Arbitrary[T3], s3: Shrink[T3],
        a4: Arbitrary[T4], s4: Shrink[T4],
        a5: Arbitrary[T5], s5: Shrink[T5],
        p: Parameters): execute.Result = {
     checkProp(asProperty(result))(p)
  }
  /** execute a ScalaCheck property */
  implicit def checkProp(prop: Prop)(implicit p: Parameters): execute.Result = checkProperty(prop)(p)
  def check(prop: Prop)(implicit p: Parameters): execute.Result = checkProp(prop)(p)
  /**
   * checks if the property is true for each generated value, and with the specified
   * generation parameters <code>p</code>. <code>p</code> is transformed into a scalacheck parameters
   * and indicates if the generation should be verbose or not
   */
  private[matcher] def checkProperty(prop: =>Prop)(implicit p: Parameters): execute.Result = {
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
    results match {
      case Result(Proved(as), succeeded, discarded, _, _) => execute.Success(noCounterExample(succeeded), succeeded)
      case Result(Passed, succeeded, discarded, _, _)     => execute.Success(noCounterExample(succeeded), succeeded)
      case r @ Result(GenException(e), n, _, _, _)        => execute.Failure(prettyTestRes(r)(defaultPrettyParams), "", e.getStackTrace().toList)
      case r @ Result(Exhausted, n, _, _, _)              => execute.Failure(prettyTestRes(r)(defaultPrettyParams))
      case Result(Failed(args, labels), n, _, _, _)       =>
        execute.Failure("A counter-example is "+counterExample(args)+" (" + afterNTries(n) + afterNShrinks(args) + ")" + failedLabels(labels))
      case Result(PropException(args, ex, labels), n, _, _, _) =>
        ex match {
          case execute.FailureException(f) =>
            execute.Failure("A counter-example is "+counterExample(args)+" (" + afterNTries(n) + afterNShrinks(args) + ")" + failedLabels(labels+f.message))
          case e: java.lang.Exception         =>
            execute.Error("A counter-example is "+counterExample(args)+": " + ex + " ("+afterNTries(n)+")"+ failedLabels(labels), e)
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
}
object ScalaCheckMatchers extends ScalaCheckMatchers
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
  def asProperty[T](f: T => MatchResult[_])
  (implicit a: Arbitrary[T], s: Shrink[T]
  ): Prop = asProperty1(f)

  implicit def asProperty1[T](f: T => MatchResult[_])
  (implicit a: Arbitrary[T], s: Shrink[T]
  ): Prop = {
	  Prop.forAll { (t: T) =>
	    f(t) match {
	   	  case MatchFailure(_, ko, _, _) => false :| ko
	   	  case _ => true :| ""
	    } 	
	  }
  }
  def asProperty[T1, T2](f: (T1, T2) => MatchResult[_])
  (implicit
      a1: Arbitrary[T1], s1: Shrink[T1],
      a2: Arbitrary[T2], s2: Shrink[T2]
  ): Prop = asProperty2(f)

  implicit def asProperty2[T1, T2](f: (T1, T2) => MatchResult[_])
  (implicit 
      a1: Arbitrary[T1], s1: Shrink[T1],
      a2: Arbitrary[T2], s2: Shrink[T2]
  ): Prop = {
    Prop.forAll { (t1: T1, t2: T2) =>
      f(t1, t2) match {
        case MatchFailure(_, ko, _, _) => false :| ko
        case _ => true :| ""
      }   
    }
  }

  def asProperty[T1, T2, T3](f: (T1, T2, T3) => MatchResult[_])
  (implicit
      a1: Arbitrary[T1], s1: Shrink[T1],
      a2: Arbitrary[T2], s2: Shrink[T2],
      a3: Arbitrary[T3], s3: Shrink[T3]
  ): Prop = asProperty3(f)

  implicit def asProperty3[T1, T2, T3](f: (T1, T2, T3) => MatchResult[_])
  (implicit 
      a1: Arbitrary[T1], s1: Shrink[T1],
      a2: Arbitrary[T2], s2: Shrink[T2],
      a3: Arbitrary[T3], s3: Shrink[T3]
  ): Prop = {
    Prop.forAll { (t1: T1, t2: T2, t3: T3) =>
      f(t1, t2, t3) match {
        case MatchFailure(ok, ko , _, _) => false :| ko
        case _ => true :| ""
      }   
    }
  }
  def asProperty[T1, T2, T3, T4](f: (T1, T2, T3, T4) => MatchResult[_])
  (implicit
      a1: Arbitrary[T1], s1: Shrink[T1],
      a2: Arbitrary[T2], s2: Shrink[T2],
      a3: Arbitrary[T3], s3: Shrink[T3],
      a4: Arbitrary[T4], s4: Shrink[T4]
  ): Prop = asProperty4(f)

  implicit def asProperty4[T1, T2, T3, T4](f: (T1, T2, T3, T4) => MatchResult[_])
  (implicit
      a1: Arbitrary[T1], s1: Shrink[T1],
      a2: Arbitrary[T2], s2: Shrink[T2],
      a3: Arbitrary[T3], s3: Shrink[T3],
      a4: Arbitrary[T4], s4: Shrink[T4]
  ): Prop = {
    Prop.forAll { (t1: T1, t2: T2, t3: T3, t4: T4) =>
      f(t1, t2, t3, t4) match {
        case MatchFailure(ok, ko , _, _) => false :| ko
        case _ => true :| ""
      }
    }
  }
  def asProperty[T1, T2, T3, T4, T5](f: (T1, T2, T3, T4, T5) => MatchResult[_])
  (implicit
      a1: Arbitrary[T1], s1: Shrink[T1],
      a2: Arbitrary[T2], s2: Shrink[T2],
      a3: Arbitrary[T3], s3: Shrink[T3],
      a4: Arbitrary[T4], s4: Shrink[T4],
      a5: Arbitrary[T5], s5: Shrink[T5]
  ): Prop = asProperty5(f)

  implicit def asProperty5[T1, T2, T3, T4, T5](f: (T1, T2, T3, T4, T5) => MatchResult[_])
  (implicit
      a1: Arbitrary[T1], s1: Shrink[T1],
      a2: Arbitrary[T2], s2: Shrink[T2],
      a3: Arbitrary[T3], s3: Shrink[T3],
      a4: Arbitrary[T4], s4: Shrink[T4],
      a5: Arbitrary[T5], s5: Shrink[T5]
  ): Prop = {
    Prop.forAll { (t1: T1, t2: T2, t3: T3, t4: T4, t5: T5) =>
      f(t1, t2, t3, t4, t5) match {
        case MatchFailure(ok, ko , _, _) => false :| ko
        case _ => true :| ""
      }
    }
  }
  /** transform a function returning a MatchResult to a property */
  protected def resultAsProperty[T](f: T => execute.Result)
  (implicit a: Arbitrary[T], s: Shrink[T]
  ): Prop = {
	  Prop.forAll { (t: T) =>
	    f(t) match {
        case execute.Failure(ko, _, _, _) => false :| ko
        case execute.Error(ko, _) => false :| ko
	   	  case _ => true :| ""
	    }
	  }
  }
  protected def resultAsProperty[T1, T2](f: (T1, T2) => execute.Result)
  (implicit
      a1: Arbitrary[T1], s1: Shrink[T1],
      a2: Arbitrary[T2], s2: Shrink[T2]
  ): Prop = {
    Prop.forAll { (t1: T1, t2: T2) =>
      f(t1, t2) match {
        case execute.Failure(ko, _, _, _) => false :| ko
        case execute.Error(ko, _) => false :| ko
        case _ => true :| ""
      }
    }
  }
  protected def resultAsProperty[T1, T2, T3](f: (T1, T2, T3) => execute.Result)
  (implicit
      a1: Arbitrary[T1], s1: Shrink[T1],
      a2: Arbitrary[T2], s2: Shrink[T2],
      a3: Arbitrary[T3], s3: Shrink[T3]
  ): Prop = {
    Prop.forAll { (t1: T1, t2: T2, t3: T3) =>
      f(t1, t2, t3) match {
        case execute.Failure(ko, _, _, _) => false :| ko
        case execute.Error(ko, _) => false :| ko
        case _ => true :| ""
      }
    }
  }
  protected def resultAsProperty[T1, T2, T3, T4](f: (T1, T2, T3, T4) => execute.Result)
  (implicit
      a1: Arbitrary[T1], s1: Shrink[T1],
      a2: Arbitrary[T2], s2: Shrink[T2],
      a3: Arbitrary[T3], s3: Shrink[T3],
      a4: Arbitrary[T4], s4: Shrink[T4]
  ): Prop = {
    Prop.forAll { (t1: T1, t2: T2, t3: T3, t4: T4) =>
      f(t1, t2, t3, t4) match {
        case execute.Failure(ko, _, _, _) => false :| ko
        case execute.Error(ko, _) => false :| ko
        case _ => true :| ""
      }
    }
  }
  protected def resultAsProperty[T1, T2, T3, T4, T5](f: (T1, T2, T3, T4, T5) => execute.Result)
  (implicit
      a1: Arbitrary[T1], s1: Shrink[T1],
      a2: Arbitrary[T2], s2: Shrink[T2],
      a3: Arbitrary[T3], s3: Shrink[T3],
      a4: Arbitrary[T4], s4: Shrink[T4],
      a5: Arbitrary[T5], s5: Shrink[T5]
  ): Prop = {
    Prop.forAll { (t1: T1, t2: T2, t3: T3, t4: T4, t5: T5) =>
      f(t1, t2, t3, t4, t5) match {
        case execute.Failure(ko, _, _, _) => false :| ko
        case execute.Error(ko, _) => false :| ko
        case _ => true :| ""
      }
    }
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
   val defaultPrettyParams = Pretty.defaultParams
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
