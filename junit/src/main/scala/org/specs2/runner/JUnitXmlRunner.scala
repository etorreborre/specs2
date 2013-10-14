package org.specs2
package runner

import reporter._

/**
 * This runner uses a JUnitXmlReporter, creating junit xml files
 *
 * @see specs2.run
 * @see org.specs2.main.Arguments for other command line options
 */
class JUnitXmlRunner extends ClassRunner {
  override lazy val reporter: Reporter = new JUnitXmlReporter {}
}