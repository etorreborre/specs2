package examples

import org.specs2.Specification
import org.specs2.specification._
import org.specs2.specification.core._
import org.specs2.specification.create.DefaultFragmentFactory
import org.specs2.time._
import org.specs2.execute._


/**
 * This specification shows various ways to setup contexts for examples.
 */
class DefineContextsSpec extends Specification:

  /**
   * This specification uses the `BeforeEach` trait to execute some code before each example
   * by simply defining a `before` method
   */
  class BeforeEachSpecification extends Specification with BeforeEach { def is = s2"""

    This is a list of examples
      example1                                  $ok
      example2                                  $ok
                                                               """
    def before =
      step(println("clean up before each example"))
  }

  /**
   * This mutable specification also uses the `BeforeEach` trait
   */
  class BeforeEachMutableSpecification extends org.specs2.mutable.Specification with BeforeEach:
    "This is a list of examples" >> {
      "example1"                 >> success
      "example2"                 >> success
    }

    def before =
      step(println("clean up before each example"))

  /**
   * This specification measure the execution time of
   * each fragment in the specification by overriding the flatMap function
   */
  class TimedExecutionSpecification extends Specification { def is = s2"""
    example 1 ${ Thread.sleep(90); ok }
    example 2 ${ Thread.sleep(10); ok }
    """

    override def flatMap(f: Fragment) =
      f.updateResult { r =>
        val (result, timer) = withTimer(ResultExecution.execute(r))
        // update the result with a piece of text which will be displayed in the consol
        result.updateExpected("Execution time: "+timer.time)
      }

    /** mesure the execution time of a piece of code */
    def withTimer[T](t: =>T): (T, SimpleTimer) =
      val timer = (new SimpleTimer).start
      val result = t
      (result, timer.stop)
  }

  def println(s: String) = () // change this definition to see messages in the console

  def is = sequential ^
    new BeforeEachSpecification ^
    new BeforeEachMutableSpecification ^
    new TimedExecutionSpecification
