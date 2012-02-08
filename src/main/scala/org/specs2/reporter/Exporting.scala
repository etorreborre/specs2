package org.specs2
package reporter

import main.Arguments
import specification._

/**
 * This trait defines a very generic way to export the result of an executing specification
 */
private[specs2]
trait Exporting {

  /** @return a function exporting an ExecutingSpecification */
  def export(implicit args: Arguments): ExecutingSpecification => ExecutedSpecification
}

