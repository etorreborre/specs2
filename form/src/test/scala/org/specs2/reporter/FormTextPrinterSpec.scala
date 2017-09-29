package org.specs2
package reporter

import specification.core.{Env, OwnEnv}
import control._
import ExecuteActions._
import form._
import main._

class FormTextPrinterSpec(val env: Env) extends Specification with specification.Forms with OwnEnv { def is = s2"""

  A form must be properly displayed in an interpolated spec $printed

"""

  def printed = {
    val logger = LineLogger.stringLogger
    val env1 = ownEnv.setLineLogger(logger).setArguments(Arguments())
    Reporter.report(env1, List(TextPrinter))(addressFormSpecStructure).runOption(ownEnv.executionEnv)
    logger.messages.mkString("\n") must contain(
    """|[info]   The address must be retrieved from the database with the proper street and number
       |[info]   + | Address           |
       |[info]     | street: Oxford St |
       |[info]     | number: 20        |""".stripMargin)
  }

  val addressFormSpecStructure = s2"""

  The address must be retrieved from the database with the proper street and number
  ${ Form("Address").
      tr(prop("street", actualStreet(123), "Oxford St")).
      tr(prop("number", actualNumber(123), 20)) }
  """

  def actualStreet(no: Int) = "Oxford St"
  def actualNumber(no: Int) = 20
}
