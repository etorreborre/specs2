package org.specs2
package specification.core

import form._
import org.specs2.main.Arguments

/**
 * Description of a Form to be used in a Fragment
 */
case class FormDescription(form: () => Form) extends Description {
  lazy val cell = new FormCell(form())
  lazy val show = cell.text

  def xml(implicit args: Arguments) = cell.xml(args)
}


