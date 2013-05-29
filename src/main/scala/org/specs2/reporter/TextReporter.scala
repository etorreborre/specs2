package org.specs2
package reporter

import specification.{ExecutedSpecification, SpecificationStructure}
import main.Arguments
import io.StringOutput
import scalaz._
import Scalaz._

/**
 * This reporter can be used to get the result of a specification execution as a String
 */
class TextReporter extends DefaultReporter with TextExporting {
  override def report(spec: SpecificationStructure)(implicit arguments: Arguments): ExecutedSpecification = {
    spec |> format |> select |> sequence |> execute |> export
  }
  override lazy val textOutput: ResultOutput with StringOutput = new TextResultOutput with StringOutput
}
