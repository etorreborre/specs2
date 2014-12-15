package org.specs2
package matcher

import org.scalacheck.Prop.Arg
import org.scalacheck._
import org.scalacheck.util.FreqMap
import org.scalacheck.util.Pretty._
import execute._
import specification._
import text.Plural._
import text.NotNullStrings._
import scalaz._, Scalaz._
import org.specs2.matcher.ScalaCheckProperty._

/**
 * A ScalaCheckProperty encapsulate a function to test with ScalaCheck
 *
 * Various typeclass instances are required:
 *
 *  - Arbitrary to generate values
 *  - Shrink to shrink counter-examples
 *  - Show to display arguments in case of a counter-example
 *  - Collector to collect values and provide a summary as string (to show frequencies for example)
 *
 *  A Context can be added to setup/teardown state before/after/around each property execution
 */
case class ScalaCheckProperty[T, R](result: T => R,
                                    arbitrary: Arbitrary[T], shrink: Option[Shrink[T]],
                                    asResult: AsResult[R],
                                    show: Show[T],
                                    collector: Collector[T],
                                    context: Option[Context],
                                    parameters: Parameters) {

  private implicit val asResult1 = asResult
  private implicit val arbitrary1 = arbitrary
  private implicit val shrink1 = shrink

  def propFunction = (t: T) => {
    lazy val r = AsResult(result(t))
    ScalaCheckProperty.asResultToProp(context.fold(r)(_(r)))
  }

  def prop: Prop =
    shrink.fold(Prop.forAllNoShrink(propFunction))(_ => Prop.forAll(propFunction))

  def noShrink: ScalaCheckProperty[T, R] =
    copy(shrink = None)

  def setArbitrary(arbitrary: Arbitrary[T]): ScalaCheckProperty[T, R] =
    copy(arbitrary = arbitrary)

  def setShrink(shrink: Shrink[T]): ScalaCheckProperty[T, R] =
    copy(shrink = Some(shrink))

  def testParameters: Test.Parameters =
    parameters.toScalaCheckParameters

  def set(minTestsOk: Int             = parameters.minTestsOk,
          minSize: Int                = parameters.minSize,
          maxDiscardRatio: Float      = parameters.maxDiscardRatio,
          maxSize: Int                = parameters.maxSize,
          workers: Int                = parameters.workers,
          rng: scala.util.Random      = parameters.rng,
          callback: Test.TestCallback = parameters.testCallback,
          loader: Option[ClassLoader] = parameters.loader
          ): ScalaCheckProperty[T, R] =
    copy(parameters =
      parameters.copy(
        minTestsOk = minTestsOk,
        minSize = minSize,
        maxDiscardRatio = maxDiscardRatio,
        maxSize = maxSize,
        workers = workers,
        rng = rng,
        callback = callback,
        loader = loader
      ))

  def collect(f: T => String): ScalaCheckProperty[T, R] =
    copy(collector = UnitReducer(f))

  def prepare(action: T => T): ScalaCheckProperty[T, R] =
    copy(result = (t: T) => result(action(t)))

  def setContext(context: Context): ScalaCheckProperty[T, R] =
    copy(context = Some(context))

  def before(action: =>Any): ScalaCheckProperty[T, R] =
    setContext(Before.create(action))

  def after(action: =>Any): ScalaCheckProperty[T, R] =
    setContext(After.create(action))

  def beforeAfter(beforeAction: =>Any, afterAction: =>Any): ScalaCheckProperty[T, R] =
    setContext(BeforeAfter.create(beforeAction, afterAction))

  def around(action: Result => Result): ScalaCheckProperty[T, R] =
    setContext(Around.create(action))
}

object ScalaCheckProperty {
  type Collector[T] = Reducer[T, String]
  
  implicit def AnyStringReducer[T]: Reducer[T, String] =
    UnitReducer((_: T).toString)

  implicit def scalaCheckPropertyAsResult[T, R]: AsResult[ScalaCheckProperty[T, R]] = new AsResult[ScalaCheckProperty[T, R]] {
    def asResult(scalaCheckProperty: =>ScalaCheckProperty[T, R]): Result =
      check(scalaCheckProperty.prop, scalaCheckProperty.testParameters, scalaCheckProperty.show, scalaCheckProperty.collector)
  }

  /**
   * checks if the property is true for each generated value, and with the specified
   * scalacheck parameters. If verbose is true, then print the results on the console
   */
  def check[T](prop: Prop, params: Test.Parameters, show: Show[T], collector: Collector[T]): execute.Result = {
    // check the property with ScalaCheck
    val result = Test.check(params, prop)
/*
    def labels(ls: collection.immutable.Set[String]) =
      if(ls.isEmpty) ""
      else "> Labels of failing property: " / ls.mkString("\n")
    val s = res.status match {
      case Test.Proved(args) => "OK, proved property."/prettyArgs(args)(prms)
      case Test.Passed => "OK, passed "+res.succeeded+" tests."
      case Test.Failed(args, l) =>
        "Falsified after "+res.succeeded+" passed tests."/labels(l)/prettyArgs(args)(prms)
      case Test.Exhausted =>
        "Gave up after only "+res.succeeded+" passed tests. " +
        res.discarded+" tests were discarded."
      case Test.PropException(args,e,l) =>
        "Exception raised on property evaluation."/labels(l)/prettyArgs(args)(prms)/
        "> Exception: "+pretty(e,prms)
    }
    val t = if(prms.verbosity <= 1) "" else "Elapsed time: "+prettyTime(res.time)
    s/t/pretty(res.freqMap,prms)

 */
    result match {
      case Test.Result(Test.Proved(as), succeeded, discarded, fq, _) =>
        execute.Success(noCounterExample(succeeded), frequencies(fq), succeeded)

      case Test.Result(Test.Passed, succeeded, discarded, fq, _)     =>
        execute.Success(noCounterExample(succeeded), frequencies(fq), succeeded)

      case r @ Test.Result(Test.Exhausted, n, _, fq, _)              =>
        execute.Failure(prettyTestRes(r) + frequencies(fq))

      case Test.Result(Test.Failed(args, labels), n, _, fq, _) =>
        new execute.Failure(counterExampleMessage(args, n, labels) + frequencies(fq), details = collectDetails(fq)) {
          // the location is already included in the failure message
          override def location = ""
        }

      case Test.Result(Test.PropException(args, ex, labels), n, _, fq, _) =>
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

  def counterExampleMessage(args: List[Prop.Arg[Any]], n: Int, labels: Set[String]) =
    "A counter-example is "+counterExample(args)+" (" + afterNTries(n) + afterNShrinks(args) + ")" + failedLabels(labels)

  // depending on the result, return the appropriate success status and messages
  // the failure message indicates a counter-example to the property
  def noCounterExample(n: Int) = "The property passed without any counter-example " + afterNTries(n)

  /**
   * Failure details might get collected with the collect operation when evaluating
   * the Prop
   */
  def collectDetails[T](fq: FreqMap[Set[T]]): execute.Details = {
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
  def removeDetails(fq: FreqMap[Set[Any]]): FreqMap[Set[Any]] = {
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


  def afterNTries(n: Int) = "after " + (if (n <= 1) n + " try" else n + " tries")

  def afterNShrinks(args: List[Arg[_]]) = {
    if (args.forall(_.shrinks == 0))  ""
    else
      args.map { arg =>
        if (arg.origArg != arg.arg) "'"+arg.origArg +"' -> '"+arg.arg+"'"
        else " = "
      }.mkString(" - shrinked (", ",", ")")
  }

  /** @return the cause of the exception as a String if there is one */
  def getCause(e: java.lang.Exception) = Option(e.getCause).map(c => "(caused by "+c+")").getOrElse("")

  def counterExample(args: List[Arg[_]]) = {
    if (args.size == 1)
      args.map(a => a.arg.notNull).mkString("'", "", "'")
    else if (args.exists(_.arg.notNull.isEmpty))
      args.map(_.arg.notNull).mkString("['", "', '", "']")
    else
      args.map(_.arg.notNull).mkString("[", ", ", "]")
  }
  def failedLabels(labels: Set[String]) = {
    if (labels.isEmpty)  ""
    else labels.mkString("\n", ", ", "\n")
  }

  def frequencies(fq: FreqMap[Set[Any]]) = {
    if (fq.getRatios.isEmpty) ""
    else "\n" //+ Pretty.prettyFreqMap(fq)(params)
  }


  def asResultToProp[R : AsResult](r: R): Prop = {
    new Prop {
      def apply(params: Gen.Parameters) = {
        lazy val result = ResultExecution.execute(AsResult(r))

        val prop =
          result match {
            case f : execute.Failure => Prop.falsified :| (f.message+" ("+f.location+")")
            case s : execute.Skipped => Prop.exception(new SkipException(s))
            case p : execute.Pending => Prop.exception(new PendingException(p))
            case e : execute.Error   => Prop.exception(e.exception)
            case other               => Prop.passed
          }
        result match {
          case f: execute.Failure if f.details != NoDetails =>
            prop.apply(params).collect(f.details)

          case _ =>
            prop.apply(params)
        }
      }
    }
  }

}