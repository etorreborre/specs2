package org.specs2
package reporter

import io._
import text.Plural._
import main.Arguments
import specification._

/**
 * This trait defines a Printer function for executed fragments and some accumulated
 * statistics
 */
private[specs2]
trait Printer extends Statistics with Output {
  def print(implicit args: Arguments): Function[(S, ExecutedFragment), ExecutedFragment]
}
