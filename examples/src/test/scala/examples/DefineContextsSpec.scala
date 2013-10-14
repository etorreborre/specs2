package examples

import org.specs2.Specification
import org.specs2.specification._
import org.specs2.time._
import org.specs2.execute._
import org.specs2.matcher.{TerminationMatchers, MustMatchers}
import org.specs2.main.CommandLineArguments
import org.specs2.time.TimeConversions._


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
   * This specification uses an implicit context for each example
   */
  class BeforeWithImplicitContextSpecification extends Specification { def is =
    s2""" $sequential

    This is a list of examples
    ${ "example1"                     ! e }
    ${ "example2"                     ! e }

    """

    var i = 0
    def e = { i += 1; i must_== 1 }
    implicit val before: Context = new Before { def before = i = 0 }
  }

  /**
   * This specification uses an implicit Outside context for each example
   */
  class OutsideWithImplicitContextSpecification extends Specification { def is =
                                                             s2"""
    This is a list of examples
      example1                                  $e1
      example2                                  $e2
                                                               """
    implicit val outside: Outside[Int] = new Outside[Int] { def outside = 1 }

    def e1 = (i: Int) => i must_== 1
    def e2 = (i: Int) => i must_== 1
  }

  /**
   * This specification uses an implicit fixture for each example
   */
  class ImplicitFixtureSpecification extends Specification { def is = s2"""
    This is a list of examples
      example1                                  $e1
      example2                                  $e2
                                                               """
    implicit val fixture: Fixture[Int] = new Fixture[Int] { def apply[R : AsResult](f: Int => R) = AsResult(f(1)) }

    def e1 = (i: Int) => i must_== 1
    def e2 = (i: Int) => i must_== 1
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
   * This specification uses the `BeforeExample` trait to execute some code before each example
   * by simply defining a `before` method
   */
  class BeforeExampleSpecification extends Specification with BeforeExample { def is = s2"""

    This is a list of examples
      example1                                  $ok
      example2                                  $ok
                                                               """
    def before = println("clean up before each example")
  }

  /**
   * This mutable specification also uses the `BeforeExample` trait
   */
  class BeforeExampleMutableSpecification extends org.specs2.mutable.Specification with BeforeExample {
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
  class TimedExecutionSpecification extends Specification with AroundContextExample[Around] { def is = s2"""
    example 1 ${ Thread.sleep(90); ok }
    example 2 ${ Thread.sleep(10); ok }
    """

    def aroundContext = new Timed {}

    trait Timed extends Around {
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
    override lazy val exampleFactory = new DefaultExampleFactory {
      override def newExample[T : AsResult](description: String, t: =>T): Example =
        super.newExample(description, context(description)(AsResult(t)))
    }
  }

  /**
   * This mutable specification shows how to reuse a trait providing an Around context which is
   * also going to use the example description. It is using a mutable Example factory for this.
   */
  class MutableTimedDescribedSpecification extends org.specs2.mutable.Specification with TimedContext {

    "Example 1" in ok
    "Example 2" in ok

    // create a new MutableExampleFactory where the body of the example uses
    // the current example description
    override lazy val exampleFactory = new MutableExampleFactory {
      override def newExample[T : AsResult](description: String, t: =>T): Example =
        super.newExample(description, context(description)(AsResult(t)))
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
  class TimeoutContextSpec extends org.specs2.mutable.Specification with ExamplesTimeout {

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

  /**
   * This shows how to create a context which will timeout any example that takes too long to execute
   * It uses the `CommandLineArguments` trait to be able to set the timeout value from the command-line
   */
  trait ExamplesTimeout extends AroundExample with MustMatchers with TerminationMatchers with CommandLineArguments {

    lazy val commandLineTimeOut = arguments.commandLine.int("timeout").map(_.millis)

    def timeout = commandLineTimeOut.getOrElse(100.millis)

    def around[T : AsResult](t: =>T) = {
      lazy val result = t
      val termination = result must terminate[T](sleep = timeout).orSkip((ko: String) => "TIMEOUT: "+timeout)
      termination.toResult and AsResult(result)
    }

  }


  def println(s: String) = s // change this definition to see messages in the console

  def is = sequential^
           new BeforeSpecification ^
           new BeforeWithImplicitContextSpecification ^
           new OutsideWithImplicitContextSpecification ^
           new BeforeMutableSpecification ^
           new BeforeExampleMutableSpecification ^
           new BeforeExampleSpecification ^
           new TimedExecutionSpecification ^
           new MutableTimedDescribedSpecification ^
           new TimedDescribedSpecification
}
