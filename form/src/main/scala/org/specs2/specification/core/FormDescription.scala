package org.specs2
package specification.core

import form._

case class FormDescription(form: () => Form) extends Description {
  lazy val show = new FormCell(form()).text
}


