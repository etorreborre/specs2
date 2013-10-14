package org.specs2
package runner

import reporter._

/**
 * This runner uses an HtmlReporter, creating html pages, for running the specification
 *
 * @see specs2.run
 * @see org.specs2.main.Arguments for other command line options
 */
class HtmlRunner extends ClassRunner {
  override lazy val reporter: Reporter = new HtmlReporter {}
}
