package org.specs2
package reporter

import main.Arguments
import specification._

/**
 * This trait defines a very generic way to export the result of an executing specification.
 *
 * The export function takes a Specification where examples might be still executing in background threads.
 * This way the results can be displayed "just-in-time" when each example has terminated its execution.
 *
 * If this functionality is not required, i.e. if all the execution can be finished before the exporting is done, the executed specification
 * can be accessed with "(s: ExecutingSpecification).execute". This call will block until all examples are executed.
 *
 */
private[specs2]
trait Exporting {

  /** @return a function exporting an ExecutingSpecification */
  def export(implicit args: Arguments): ExecutingSpecification => ExecutedSpecification
}

/**
 * public trait to create a custom exporter
 */
trait Exporter extends Exporting

/**
 * Null implementation for the Exporting trait
 */
private [specs2]
trait NoExporting extends Exporting {
  def export(implicit args: Arguments) = (s: ExecutingSpecification) => s.execute
}
private [specs2]
object NoExporting extends NoExporting

