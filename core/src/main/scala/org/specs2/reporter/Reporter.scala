package org.specs2
package reporter

import scalaz._
import Scalaz._
import main.Arguments
import reflect.Classes
import specification._

/**
 * A Reporter will report the execution of a Specification following 3 steps:
 * 
 * 1. an extraction of the Fragments to report (like creating Descriptions for JUnit)
 *   - filtering out some elements if necessary
 *   
 * 2. an ordering of the Fragments to execute:
 *   - action steps must be executed in order
 *   - dependency between Fragments can be specified
 *   - other Fragments can be executed concurrently (unless specified otherwise)
 *   
 * 3. a reporting to:
 *   - the console (ConsoleRunner or sbt)
 *   - a listener object (junit or sbt)
 *   - a file (html, xml, junit-report)
 *
 */
trait Reporter extends
       Selection
  with Sequence
  with ExecutionStrategy
  with Storing
  with Exporting {

  /**
   * report Fragments by:
   *   - extracting arguments from the Fragments
   *   - selecting them
   *   - sequencing fragments in groups
   *   - executing fragments
   *   - exporting the results to the output format
   *   
   * @return the reporter
   */
  def report(spec: SpecificationStructure)(implicit arguments: Arguments): ExecutedSpecification = {
    spec |> select |> sequence |> execute |> store |> export
  }
}

trait DefaultReporter extends Reporter
  with Executor
  with DefaultSelection
  with DefaultSequence
  with DefaultExecutionStrategy
  with DefaultStoring {

  override def select(implicit arguments: Arguments)   = delegate(arguments).map(_.select(arguments)).getOrElse(super.select(arguments))
  override def sequence(implicit arguments: Arguments) = delegate(arguments).map(_.sequence(arguments)).getOrElse(super.sequence(arguments))
  override def execute(implicit arguments: Arguments)  = delegate(arguments).map(_.execute(arguments)).getOrElse(super.execute(arguments))
  override def store(implicit arguments: Arguments)    = delegate(arguments).map(_.store(arguments)).getOrElse(super.store(arguments))
}

trait Executor extends
       DefaultSelection
  with DefaultSequence
  with DefaultExecutionStrategy
  with DefaultStoring { self =>

  def delegate(arguments: Arguments) =
    if (arguments.execute.executor.nonEmpty) Classes.createObject[Executor](arguments.execute.executor, printMessage = true)
    else None
}

