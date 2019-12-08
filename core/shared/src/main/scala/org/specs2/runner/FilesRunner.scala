package org.specs2
package runner

import main._
import control._
import io.DirectoryPath
import reporter.PrinterLogger._
import specification.process.Stats
import specification.core._
import reporter._
import runner.Runner._
import org.specs2.fp.syntax._
import SpecificationsFinder._

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
    val env = Env(arguments = Arguments(args: _*),
      printerLogger = consolePrinterLogger)

    try     execute(run(env), env.arguments, exit)(env)
    finally env.shutdown
  }

  def run(env: Env): Action[Stats] = {
    val args = env.arguments
    val base = args.commandLine.valueOr("filesrunner.basepath", new java.io.File(specificationsBasePath).getAbsolutePath)
    val specs = for {
      basePath <- Action.checkThat(base, new java.io.File(base).isDirectory, s"$base must be a directory")
      ss <- findSpecifications(
        glob = args.commandLine.valueOr("filesrunner.path", specificationsPath),
        pattern = args.commandLine.valueOr("filesrunner.pattern", specificationsPattern),
        basePath = DirectoryPath.unsafe(basePath),
        verbose = isVerbose(args)).toAction
    } yield ss

    val logger = ConsoleLogger()

    for {
      _     <- beforeExecution(args, isVerbose(args), logger).toAction
      ss    <- specs.map(sort(env))
      cr    <- ClassRunner.createClassRunner(args, env)
      stats <- ss.toList.traverse(cr.run)
      _     <- afterExecution(ss, isVerbose(args), logger).toAction
    } yield stats.suml
  }

  /** sort the specifications in topological order where specification i doesn't depend on specification j if i > j == dependents first */
  def sort(env: Env) = { specifications: Seq[SpecificationStructure] =>
    SpecificationStructure.topologicalSort(env)(specifications).getOrElse(specifications)
  }

  /** @return true if the output must be verbose for debugging */
  def isVerbose(args: Arguments) = args.isSet("filesrunner.verbose")

  /** print a message before the execution */
  protected def beforeExecution(args: Arguments, verbose: Boolean, logger: Logger = ConsoleLogger()): Operation[Unit] = for {
    _        <- logger.info("\nExecuting specifications", verbose)
    printers <- PrinterFactory.create(args).createPrinters
    _        <- logger.info("printers are " + printers.mkString(", "), verbose)
  } yield ()


  /** print a message after the execution based on the number of specifications */
  protected def afterExecution(specs: Seq[SpecificationStructure], verbose: Boolean, logger: Logger = ConsoleLogger()): Operation[Unit] = {
    if (specs.isEmpty) logger.info("No specification found\n", verbose)
    else               logger.info("Finished the execution of " + specs.size + " specifications\n", verbose)
  }
}

/**
 * Run specification files from the command line with specs2.files <specification name> <arguments>
 */
object files extends FilesRunner {
  def main(args: Array[String]) =
    run(args, exit = true)
}
