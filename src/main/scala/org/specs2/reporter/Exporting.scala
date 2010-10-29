package org.specs2
package reporter
import specification._

trait Exporting {
  type ExportType
  def export(implicit args: Args): List[ExecutedFragment] => ExportType
}

trait FolderExporting {
  val folder: Folder[ExecutedFragment]
  type ExportType = folder.T
  def export(implicit args: Args) = (fragments: List[ExecutedFragment]) => folder.fold(fragments)
}
