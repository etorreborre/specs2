package examples

import org.specs2.mutable.Specification

/**
 * This specification shows how to create an "Around" context which will time out every example
 */
class TimeoutContextSpec extends Specification with ExamplesTimeout {

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

import org.specs2.specification.AroundExample
import org.specs2.execute._
import org.specs2.matcher.{TerminationMatchers, MustMatchers}
import org.specs2.main.CommandLineArguments
import org.specs2.time.TimeConversions._

trait ExamplesTimeout extends AroundExample with MustMatchers with TerminationMatchers with CommandLineArguments {

  lazy val commandLineTimeOut = arguments.commandLine.int("timeout").map(_.millis)

  def timeout = commandLineTimeOut.getOrElse(100.millis)

  def around[T : AsResult](t: =>T) = {
    lazy val result = t
    val termination = result must terminate[T](sleep = timeout).orSkip((ko: String) => "TIMEOUT: "+timeout)
    termination.toResult and AsResult(result)
  }

}