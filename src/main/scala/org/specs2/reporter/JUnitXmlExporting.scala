package org.specs2
package reporter

import main.Arguments
import specification._

/**
 * Trait for exporting the specification as JUnit xml files
 */
trait JUnitXmlExporting extends Exporting with JUnitXmlPrinter {
  type ExportType = Unit

  def export(s: SpecificationStructure)(implicit args: Arguments) = (fragments: Seq[ExecutedFragment]) => {
    print(s, fragments)
  }

}
