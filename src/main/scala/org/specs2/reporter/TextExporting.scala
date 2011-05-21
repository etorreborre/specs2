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
  
  def export(s: SpecificationStructure)(implicit args: Arguments) = (fragments: Seq[ExecutedFragment]) => {
    print(s, fragments)
  }
} 
