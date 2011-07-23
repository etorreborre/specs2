package org.specs2
package reporter

import io.ConsoleOutput

/**
 * This trait execute specifications and exports them as JUnit xml files in the target/test-reports directory (by default).
 */
trait JUnitXmlReporter extends DefaultReporter
  with JUnitXmlExporting
  with ConsoleOutput
