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
  def export(implicit args: Arguments): Seq[ExecutedFragment] => ExportType
}

/**
 * This trait uses an ExecutedFragmentFold to produce the exported type
 */
private[specs2]
trait FoldExporting {
  type ExportType = fold.T

  val fold: ExecutedFragmentFold
  /** @return a function exporting ExecutedFragments */
  def export(implicit args: Arguments) = (fragments: Seq[ExecutedFragment]) => fold.foldAll(fragments)
}
