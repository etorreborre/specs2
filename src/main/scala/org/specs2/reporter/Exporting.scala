package org.specs2
package reporter

import main.Arguments
import specification._

/**
 * This trait defines a very generic way to export the result of an executed specification
 */
private[specs2]
trait Exporting {

  type ExportType
  
  /** @return a function exporting ExecutedFragments */
  def export(klass: Class[_])(implicit args: Arguments): Seq[ExecutedFragment] => ExportType
}
