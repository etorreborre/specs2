package org.specs2
package runner
import reporter._

class HtmlRunner extends ClassRunner {
  override lazy val reporter: Reporter = new HtmlReporter {}
}