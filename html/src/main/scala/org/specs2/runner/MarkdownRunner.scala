package org.specs2
package runner

import reporter._

/**
 * This runner uses a MarkdownReporter, creating markdown pages, for running the specification
 *
 * @see specs2.run
 * @see org.specs2.main.Arguments for other command line options
 */
class MarkdownRunner extends ClassRunner {
  override lazy val reporter: Reporter = new MarkdownReporter {}
}