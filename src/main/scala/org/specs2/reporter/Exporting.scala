package org.specs2
package reporter

import main.Arguments
import specification._

private[specs2]
trait Exporting {
  type ExportType
  def export(implicit args: Arguments): Seq[ExecutedFragment] => ExportType
}

private[specs2]
trait FoldExporting {
  val fold: ExecutedFragmentFold
  type ExportType = fold.T
  def export(implicit args: Arguments) = (fragments: Seq[ExecutedFragment]) => fold.foldAll(fragments)
}
