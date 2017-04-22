package org.specs2
package reporter

import specification.core.Env
import control._
import form._
import main._
import org.specs2.concurrent.ExecutionEnv

class FormTextPrinterSpec(ee: ExecutionEnv) extends Specification with specification.Forms { def is = s2"""

  A form must be properly displayed in an interpolated spec $printed

"""

  def printed = { (env1: Env) =>
    val logger = LineLogger.stringLogger
    val env = env1.setLineLogger(logger).setArguments(Arguments())
    Reporter.report(env, List(TextPrinter))(addressFormSpecStructure).runOption(ee)
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
