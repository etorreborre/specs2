package org.specs2
package reporter

import collection.Iterablex._
import main.Arguments
import specification._

/**
 * This trait defines a very generic way to export the result of an executed specification
 */
private[specs2]
trait Exporting {

  type ExportType

  /** @return a function exporting an ExecutedSpecification */
  def export(implicit args: Arguments): ExecutedSpecification => ExportType
}

