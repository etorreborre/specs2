package org.specs2
package specification

import form._
import text._

/**
 * Allow a Form to be inserted among Fragments as a Text Fragment
 * Allow a Form to be used as an example body and return a Result automatically
 */
trait Forms extends FormsBuilder with DecoratedProperties {
  class FormExample(form: Form) extends Example(FormMarkup(form), () => form.execute)
  implicit def formsAreExamples(f: Form): Example = new FormExample(f)
  implicit def formsHoldersAreExamples(f: { def form: Form }): Example = formsAreExamples(f.form)
}
object Forms extends Forms

/**
 * The FormMarkup embbeds the description of a form as text or as Xml
 */
case class FormMarkup(form: Form) extends MarkupString {
  def toXml = form.toXml
  override def toString = new FormCell(form).text
}
