package org.specs2
package runner

import main._
import control._
import io.DirectoryPath
import reporter.LineLogger._
import specification.process.Stats
import specification.core._
import runner.Runner._
import scalaz._, Scalaz._
import SpecificationsFinder._
import Actions._

/**
 * This trait finds specifications in the source directory, instantiate them
 * and report them using various printers as specified on the command line
 *
 */
trait FilesRunner {

  /**
   * Run the specifications found in files based on command-line arguments
   */
  def run(args: Array[String], exit: Boolean = false) {
    val env = Env(arguments = Arguments(args: _*),
      lineLogger = consoleLogger)

    val actions: Action[Stats] =
      for {
        stats <- run(env)
        _     <- Actions.delayed(env.shutdown)
      } yield stats

      execute(actions, env.arguments, exit)
  }

  def run(env: Env): Action[Stats] = {
    val args = env.arguments
    val base = args.commandLine.valueOr("filesrunner.basepath", new java.io.File(specificationsBasePath).getAbsolutePath)
    val specs = for {
      basePath <- Actions.checkThat(base, new java.io.File(base).isDirectory, s"$base must be a directory")
      ss <- findSpecifications(
        glob = args.commandLine.valueOr("filesrunner.path", specificationsPath),
        pattern = args.commandLine.valueOr("filesrunner.pattern", specificationsPattern),
        basePath = DirectoryPath.unsafe(basePath),
        verbose = isVerbose(args))
    } yield ss

    for {
      _     <- beforeExecution(args, isVerbose(args))
      ss    <- specs.map(sort(env))
      stats <- ss.toList.map(ClassRunner.report(env)).sequenceU
      _     <- afterExecution(ss, isVerbose(args))
    } yield stats.foldMap(identity _)
  }

  /** sort the specifications in topological order where specification i doesn't depend on specification j if i > j == dependents first */
  def sort(env: Env) = { specifications: Seq[SpecificationStructure] =>
    SpecificationStructure.topologicalSort(env)(specifications).getOrElse(specifications)
  }

  /** @return true if the output must be verbose for debugging */
  def isVerbose(args: Arguments) = args.isSet("filesrunner.verbose")

  /** print a message before the execution */
  protected def beforeExecution(args: Arguments, verbose: Boolean): Action[Unit] = for {
    _ <- log("\nExecuting specifications", verbose)
    printers <- ClassRunner.createPrinters(args, Thread.currentThread.getContextClassLoader)
    _ <- log("printers are " + printers.mkString(", "), verbose)
  } yield ()


  /** print a message after the execution based on the number of specifications */
  protected def afterExecution(specs: Seq[SpecificationStructure], verbose: Boolean): Action[Unit] = {
    if (specs.isEmpty) log("No specification found\n", verbose)
    else log("Finished the execution of " + specs.size + " specifications\n", verbose)
  }
}

/**
 * Run specification files from the command line with specs2.files <specification name> <arguments>
 */
object files extends FilesRunner {
  def main(args: Array[String]) =
    run(args, exit = true)
}
