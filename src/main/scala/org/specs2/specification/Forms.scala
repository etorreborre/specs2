package org.specs2
package specification

import form._
/**
 * Allow a Form to be inserted among Fragments as a Text Fragment
 * Allow a Form to be used as an example body and return a Result automatically
 */
private[specs2]
trait Forms extends FormsBuilder {
  class FormFragment(form: Form) extends Text(FormCell(form).text)
  implicit def formsAreFragments(f: Form): Fragment = new FormFragment(f)
}
private[specs2]
object Forms extends Forms