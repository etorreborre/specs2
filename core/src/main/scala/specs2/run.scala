package specs2

import org.specs2.control.Actions
import org.specs2.main.Arguments
import org.specs2.runner.{Runner, ClassRunner}
import org.specs2.specification.core.{Env, SpecificationStructure}
import scalaz._, Scalaz._

object run extends ClassRunner {

  /**
   * run one or more specifications with `specs2.run(spec1, spec2)` from the console
   */
  def apply(specifications: SpecificationStructure*)(implicit arguments: Arguments = Arguments()) = {
    val env = Env(arguments = arguments)
    try     Runner.execute(specifications.toList.map(report(env)).sequenceU.void)
    finally env.shutdown
  }

  /** main method for the command line */
  def main(args: Array[String]) =
    run(args)
}
