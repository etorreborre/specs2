package examples

import org.specs2.Specification
import org.specs2.specification._
import org.specs2.specification.create.{DefaultFragmentFactory, FragmentFactory}
import org.specs2.time._
import org.specs2.execute._
import org.specs2.matcher.{TerminationMatchers, MustMatchers}
import scala.concurrent.duration._


/**
 * This specification shows various ways to setup contexts for examples.
 *
 * For more details @see org.specs2.guide.Structure
 */
class DefineContextsSpec extends Specification {

  /**
   * This specification uses a context class extending the `Before` trait.
   * It is also creating "fresh" variables for each example
   */
  class BeforeSpecification extends Specification { def is = s2"""

    This is a list of examples
      example1                                  ${clean().e1}
      example2                                  ${clean().e2}
                                                               """

    case class clean() extends Before {
      val aNewSystem = "a fresh value"
      def before = println("clean up before each example")

      def e1 = this { aNewSystem must_== "a fresh value" }
      def e2 = this { aNewSystem must_== "a fresh value" }
    }
  }

  /**
   * This specification shows how to use the mutable.Before trait in a mutable specification
   */
  class BeforeMutableSpecification extends org.specs2.mutable.Specification {
    "This is a list of examples" >> {
      "example1" >> new clean {
        aNewSystem must_== "a fresh value"
      }
      "example2" >> new clean {
        aNewSystem must_== "a fresh value"
      }
    }

    /** here we need a trait extending mutable.Before because the example body will be executed as a "delayed init"  section*/
    trait clean extends org.specs2.mutable.Before {
      lazy val aNewSystem = "a fresh value"
      def before = println("clean up before each example")
    }
  }

  /**
   * This specification uses the `BeforeEach` trait to execute some code before each example
   * by simply defining a `before` method
   */
  class BeforeEachSpecification extends Specification with BeforeEach { def is = s2"""

    This is a list of examples
      example1                                  $ok
      example2                                  $ok
                                                               """
    def before = println("clean up before each example")
  }

  /**
   * This mutable specification also uses the `BeforeEach` trait
   */
  class BeforeEachMutableSpecification extends org.specs2.mutable.Specification with BeforeEach {
    "This is a list of examples" >> {
      "example1"                 >> success
      "example2"                 >> success
    }

    def before = println("clean up before each example")
  }

  /**
   * This specification shows how to create an Around context that will measure the execution time of
   * each example and update the result message
   */
  class TimedExecutionSpecification extends Specification with AroundEach { def is = s2"""
    example 1 ${ Thread.sleep(90); ok }
    example 2 ${ Thread.sleep(10); ok }
    """

    def around[T : AsResult](t: =>T): Result = {
      // use `ResultExecution.execute` to catch possible exceptions
      val (result, timer) = withTimer(ResultExecution.execute(AsResult(t)))

      // update the result with a piece of text which will be displayed in the console
      result.updateExpected("Execution time: "+timer.time)
    }

    /** mesure the execution time of a piece of code */
    def withTimer[T](t: =>T): (T, SimpleTimer) = {
      val timer = (new SimpleTimer).start
      val result = t
      (result, timer.stop)
    }

  }

  /**
   * This specification shows how to reuse a trait providing an Around context which is
   * also going to use the example description. It is using an Example factory for this.
   */
  class TimedDescribedSpecification extends Specification with TimedContext { def is = s2"""
      Example 1 $ok
      Example 2 $ok
    """
    // create a new DefaultExampleFactory where the body of the example uses
    // the current example description
    override lazy val fragmentFactory = new DefaultFragmentFactory {
      override def example[T : AsResult](description: String, t: =>T) =
        super.example(description, context(description)(AsResult(t)))
    }
  }

  /**
   * This trait provides an Around context to a trait measure an example execution time
   */
  trait TimedContext {
    def context(exampleDescription: String) = new Timed(exampleDescription)

    case class Timed(exampleDescription: String) extends Around {
      def around[T : AsResult](t: =>T): Result = {
        val (result, timer) = withTimer(ResultExecution.execute(AsResult(t)))
        result.updateExpected(s"Execution time for example $exampleDescription: ${timer.time}")
      }

      /** mesure the execution time of a piece of code */
      def withTimer[T](t: =>T): (T, SimpleTimer) = {
        val timer = (new SimpleTimer).start
        val result = t
        (result, timer.stop)
      }
    }
  }

  /**
   * This specification shows how to create an "Around" context which will time out every example
   */
  class TimeoutContextSpec extends org.specs2.mutable.Specification  {

    "This example should pass" >> {
      { Thread.sleep(50); 1 } must_== 1
    }
    //  "This example should timeout" >> {
    //    def loop: Unit = loop;
    //    { loop; 1 } must_== 1
    //  }
    //  "This example should fail" >> {
    //    { Thread.sleep(50); 2 } must_== 1
    //  }

  }

//  /**
//   * This shows how to create a context which will timeout any example that takes too long to execute
//   * It uses the `CommandLineArguments` trait to be able to set the timeout value from the command-line
//   */
//  trait ExamplesTimeout extends AroundExample with MustMatchers with TerminationMatchers {
//
////    lazy val commandLineTimeOut = arguments.commandLine.int("timeout").map(_.millis)
//
//    def timeout = //commandLineTimeOut.getOrElse(
//      100.millis //)
//
//    def around[T : AsResult](t: =>T) = {
//      lazy val result = t
//      val termination = result must terminate[T](sleep = timeout).orSkip((ko: String) => "TIMEOUT: "+timeout)
//      termination.toResult and AsResult(result)
//    }
//
//  }


  def println(s: String) = s // change this definition to see messages in the console

  def is = sequential ^
           new BeforeSpecification ^
           new BeforeMutableSpecification ^
           new BeforeEachMutableSpecification ^
           new BeforeEachSpecification ^
           new TimedExecutionSpecification ^
           new TimedDescribedSpecification
}
