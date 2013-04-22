package org.specs2
package runner

import reporter.TextReporter
import specification.Fragments
import control.Exceptions._
import main.Arguments

class TextRunner extends org.specs2.runner.ClassRunner {
  override lazy val reporter: TextReporter = new TextReporter {}

  def apply(fs: Fragments): String = {
    val specification = new Specification { def is = fs }
    tryo(reporter.report(specification)(Arguments()))(errorHandler)
    output
  }

  /** @return the output of one run */
  protected def output = {
    val result = reporter.textOutput.output
    reporter.textOutput.clear
    result
  }
}

