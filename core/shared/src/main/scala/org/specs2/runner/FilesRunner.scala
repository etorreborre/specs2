package org.specs2
package runner

import main._
import control._
import io.DirectoryPath
import specification.process.Stats
import specification.core._
import reporter._
import runner.Runner._
import org.specs2.fp.syntax._
import SpecificationsFinder._

trait FilesRunner {
  /** run any specifications found via arguments */
  def run: Action[Stats]
}

case class DefaultFilesRunner(env: Env, specificationsFinder: SpecificationsFinder) extends FilesRunner {

  val logger = env.systemLogger
  val arguments = env.arguments

  def run: Action[Stats] = {
    val base = arguments.commandLine.valueOr("filesrunner.basepath", new java.io.File(specificationsBasePath).getAbsolutePath)

    val specs = for {
      basePath <- Action.checkThat(base, new java.io.File(base).isDirectory, s"$base must be a directory")
      ss <- specificationsFinder.findSpecifications(
        glob = arguments.commandLine.valueOr("filesrunner.path", specificationsPath),
        pattern = arguments.commandLine.valueOr("filesrunner.pattern", specificationsPattern),
        basePath = DirectoryPath.unsafe(basePath),
        verbose = isVerbose).toAction
    } yield ss

    for {
      _     <- beforeExecution.toAction
      ss    <- specs.map(sort)
      cr    <- ClassRunner.createClassRunner(env)
      stats <- ss.toList.traverse(cr.run)
      _     <- afterExecution(ss).toAction
    } yield stats.suml
  }

  /** sort the specifications in topological order where specification i doesn't depend on specification j if i > j == dependents first */
  def sort = { specifications: Seq[SpecificationStructure] =>
    SpecificationStructure.topologicalSort(env)(specifications).getOrElse(specifications)
  }

  /** @return true if the output must be verbose for debugging */
  def isVerbose: Boolean =
    arguments.isSet("filesrunner.verbose")

  /** print a message before the execution */
  protected def beforeExecution: Operation[Unit] = for {
    _        <- logger.info("\nExecuting specifications", isVerbose)
    printers <- PrinterFactory.create(arguments).createPrinters
    _        <- logger.info("printers are " + printers.mkString(", "), isVerbose)
  } yield ()


  /** print a message after the execution based on the number of specifications */
  protected def afterExecution(specs: Seq[SpecificationStructure]): Operation[Unit] = {
    if (specs.isEmpty) logger.info("No specification found\n", isVerbose)
    else               logger.info("Finished the execution of " + specs.size + " specifications\n", isVerbose)
  }
}

/**
 * This trait finds specifications in the source directory, instantiate them
 * and report them using various printers as specified on the command line
 *
 */
trait FilesRunnerMain {

  /**
   * Run the specifications found in files based on command-line arguments
   */
  def run(args: Array[String], exit: Boolean = false): Unit = {
    val env = EnvDefault.create(Arguments(args: _*))
    val specificationsFinder = DefaultSpecificationsFinder(env)
    try     execute(DefaultFilesRunner(env, specificationsFinder).run, env, exit)
    finally env.shutdown
  }
}

/**
 * Run specification files from the command line with specs2.files <specification name> <arguments>
 */
object files extends FilesRunnerMain {
  def main(args: Array[String]) =
    run(args, exit = true)
}
