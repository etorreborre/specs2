package org.specs2
package runner

import reporter._
import main.Arguments
import specification.{ExecutedSpecification, ExecutingSpecification}

/**
 * This runner can be used with any class implementing the Notifier trait
 */
case class NotifierRunner(notifier: Notifier) { outer =>

  def classRunner = new ClassRunner {
    override lazy val reporter: Reporter = new NotifierReporter {
      val notifier = outer.notifier
      override def export(implicit arguments: Arguments): ExecutingSpecification => ExecutedSpecification = (spec: ExecutingSpecification) => {
        super.export(arguments)(spec)
        exportAll(arguments)(spec)
        spec.executed
      }
    }
  }

  def main(arguments: Array[String])  = classRunner.main(Array(arguments:_*))
  def start(arguments: Array[String]) = classRunner.start(arguments:_*)
}