package org.specs2
package reporter
import specification._
import io._
import text.Plural._

trait Printer extends Statistics with Output {
  def print(implicit args: Args): Function[(S, ExecutedFragment), ExecutedFragment]
}
