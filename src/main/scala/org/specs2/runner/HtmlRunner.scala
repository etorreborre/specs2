package org.specs2
package runner
import reporter._

/**
 * This runner uses an HtmlReporter, creating html pages, for running the specification 
 * @author etorrebo
 *
 */
class HtmlRunner extends ClassRunner {
  override lazy val reporter: Reporter = new HtmlReporter {}
}