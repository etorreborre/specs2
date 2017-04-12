package specs2

import org.specs2.main.Arguments
import org.specs2.reporter.LineLogger._
import org.specs2.runner.{Runner, ClassRunner}
import org.specs2.specification.core.{Env, SpecificationStructure}
import org.specs2.fp.syntax._

/**
 * Run a specification from the command-line with specs2.run <specification name> <arguments>
 */
object run extends ClassRunner {

  /**
   * Run one or more specifications with `specs2.run(spec1, spec2)` from a terminal
   */
  def apply(specifications: SpecificationStructure*)(implicit arguments: Arguments = Arguments()) = {
    val env = Env(arguments = arguments, lineLogger = consoleLogger)

    try     Runner.execute(specifications.toList.traverse(report(env)).map(_.suml), arguments, exit = false)(env)
    finally env.shutdown
  }

  /** main method for the command line */
  def main(args: Array[String]) =
    run(args, exit = true)
}
