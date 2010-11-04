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
trait FolderExporting {
  val folder: Folder[ExecutedFragment]
  type ExportType = folder.T
  def export(implicit args: Arguments) = (fragments: Seq[ExecutedFragment]) => folder.fold(fragments)
}
