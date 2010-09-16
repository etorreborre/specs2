package org.specs2
package runner
import specification._
import io._
import text.Plural._

trait Printer extends Statistics with Output {
  val print: Function[(S, ExecutedFragment), ExecutedFragment]
}
