package org.specs2
package runner

import main._
import control._
import io.DirectoryPath
import reporter.LineLogger._
import specification.process.Stats
import specification.core._
import runner.Runner._
import org.specs2.fp.syntax._
import SpecificationsFinder._
import Operations._

/**
 * This trait finds specifications in the source directory, instantiate them
 * and report them using various printers as specified on the command line
 *
 */
trait FilesRunner {

  /**
   * Run the specifications found in files based on command-line arguments
   */
  def run(args: Array[String], exit: Boolean = false): Unit = {
    val arguments = Arguments(args: _*)
    arguments.reportUnknown()
    val env = Env(arguments = arguments, lineLogger = consoleLogger)

    try     execute(run(env), env.arguments, exit)(env)
    finally env.shutdown
  }

  def run(env: Env): Action[Stats] = {
    val args = env.arguments
    val filesRunnerArguments = FilesRunnerArguments.extract(args)
    val verbose = filesRunnerArguments.verbose
    val base = filesRunnerArguments.basePath
    val specs = for {
      basePath <- Actions.checkThat(base, new java.io.File(base).isDirectory, s"$base must be a directory")
      ss <- findSpecifications(
        glob = filesRunnerArguments.glob,
        pattern = filesRunnerArguments.pattern,
        basePath = DirectoryPath.unsafe(basePath),
        verbose = verbose).toAction
    } yield ss

    for {
      _     <- beforeExecution(args, verbose).toAction
      ss    <- specs.map(sort(env))
      stats <- ss.toList.map(ClassRunner.report(env)).sequence
      _     <- afterExecution(ss, verbose).toAction
    } yield stats.foldMap(identity _)
  }

  /** sort the specifications in topological order where specification i doesn't depend on specification j if i > j == dependents first */
  def sort(env: Env) = { specifications: Seq[SpecificationStructure] =>
    SpecificationStructure.topologicalSort(env)(specifications).getOrElse(specifications)
  }

  /** print a message before the execution */
  protected def beforeExecution(args: Arguments, verbose: Boolean): Operation[Unit] = for {
    _        <- log("\nExecuting specifications", verbose)
    printers <- ClassRunner.createPrinters(args, Thread.currentThread.getContextClassLoader)
    _        <- log("printers are " + printers.map(_.getClass).mkString(", "), verbose)
  } yield ()


  /** print a message after the execution based on the number of specifications */
  protected def afterExecution(specs: Seq[SpecificationStructure], verbose: Boolean): Operation[Unit] = {
    if (specs.isEmpty) log("No specification found\n", verbose)
    else               log("Finished the execution of " + specs.size + " specifications\n", verbose)
  }
}
