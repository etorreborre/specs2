package org.specs2
package reporter

import io.Output
import main.Arguments
import specification._

/**
 * This trait prints the executed fragments results and statistics
 * at the end of the specification
 */
trait TextExporting extends TextPrinter with Exporting { outer =>

  type ExportType = Unit
  
  def export(implicit args: Arguments): ExecutingSpecification => ExportType = (spec: ExecutingSpecification) => {
    spec.foreach { (name, fragments) =>  print(name, fragments) }
  }
} 
