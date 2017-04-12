package org.specs2
package runner

import reporter._

/**
 * This runner can be used with any class implementing the Notifier trait
 */
case class NotifierRunner(notifier: Notifier) {
  def main(arguments: Array[String])  =
    try {
      ClassRunner.run(Array(arguments ++ Seq("notifier", notifier.getClass.getName):_*))
    } catch { case t: Throwable => t.printStackTrace }
}
