package org.specs2
package specification.core

import form.*
import main.Arguments
import text.Indent.*

/**
 * Description of a Form to be used in a Fragment
 */
case class FormDescription(form: () => Form) extends Description:

  lazy val cell = new FormCell(form())
  lazy val show = cell.text

  def xml(using args: Arguments) =
    cell.xml

  def indent(spaces: String): FormDescription =
    new FormDescription(form) {
      override lazy val show =
        indentAllButFirstLine(cell.text, spaces)
    }
