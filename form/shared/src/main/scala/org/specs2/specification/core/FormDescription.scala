package org.specs2
package specification.core

import form._
import main.Arguments
import text.Indent._

/**
 * Description of a Form to be used in a Fragment
 */
case class FormDescription(form: () => Form) extends Description {

  lazy val cell = new FormCell(form())
  lazy val show = cell.text

  def xml(implicit args: Arguments) = cell.xml(args)

  def indent(spaces: String) =
    new FormDescription(form) {
      override lazy val show =
        indentAllButFirstLine(cell.text, spaces)
    }
}


