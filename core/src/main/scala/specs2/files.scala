package specs2

import org.specs2.main.*
import org.specs2.control.*
import org.specs2.runner.*
import org.specs2.specification.core.*
import org.specs2.specification.process.*

/**
 * Run specification files from the command line with specs2.files <specification name> <arguments>
 */
object files:
  def main(args: Array[String]) =
    run(args, exit = true)

  def run(args: Array[String], exit: Boolean): Unit =
    val arguments = Arguments(args.drop(1)*)
    val env = EnvDefault.create(arguments)

    val filesRunner = DefaultFilesRunner(env, SpecificationsFinder.default)
    try Runner.execute(filesRunner.run, env, exit)
    finally env.shutdown()
