package org.specs2
package runner

import reporter._

/**
 * This runner can be used with any class implementing the Notifier trait
 */
case class NotifierRunner(notifier: Notifier) extends ClassRunner { outer =>
  override lazy val reporter: Reporter = new NotifierReporter {
    val notifier = outer.notifier
  }
}