package org.specs2
package runner

import reporter.TextReporter
import specification.Fragments
import control.Exceptions._
import main.Arguments

class TextRunner extends org.specs2.runner.ClassRunner {
  override lazy val reporter: TextReporter = new TextReporter {}

  def apply(fs: Fragments): String = ???

  /** @return the output of one run */
  protected def output = {
    val result = reporter.textOutput.output
    reporter.textOutput.clear
    result
  }
}

