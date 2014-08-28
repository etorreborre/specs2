package specs2

import org.specs2.control.Actions
import org.specs2.main.Arguments
import org.specs2.runner.{Runner, ClassRunner}
import org.specs2.specification.core.{Env, SpecificationStructure}
import scalaz._, Scalaz._

object run extends ClassRunner {

  /**
   * Run one or more specifications with `specs2.run(spec1, spec2)` from a terminal
   * if you use the sbt console you need to pass the `noexit` argument: specs2.run(spec1)(Arguments("noexit"))
   * otherwise the session will exit with System.exit
   */
  def apply(specifications: SpecificationStructure*)(implicit arguments: Arguments = Arguments()) = {
    val env = Env(arguments = arguments)
    try     Runner.execute(specifications.toList.map(report(env)).sequenceU.void, arguments)
    finally env.shutdown
  }

  /** main method for the command line */
  def main(args: Array[String]) =
    run(args)
}
