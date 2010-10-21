package org.specs2
package reporter
import specification._

trait Exporting {
  type ExportType
  val export: List[ExecutedFragment] => ExportType
}

trait FolderExporting {
  val folder: Folder[ExecutedFragment]
  type ExportType = folder.T
  val export = (fragments: List[ExecutedFragment]) => folder.fold(fragments)
}
