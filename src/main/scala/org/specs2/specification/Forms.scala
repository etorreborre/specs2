package org.specs2
package specification

import form._
import text._

/**
 * Allow a Form to be inserted among Fragments as a Text Fragment
 * Allow a Form to be used as an example body and return a Result automatically
 */
private[specs2]
trait Forms extends FormsBuilder {
  class FormExample(form: Form) extends Example(FormMarkup(form), () => form.execute)
  implicit def formsAreExamples(f: Form): Example = new FormExample(f)
  implicit def formsHoldersAreExamples(f: { def form: Form }): Example = formsAreExamples(f.form)
}

/**
 * The FormMarkup embbeds the description of a form as text or as Xml
 */
case class FormMarkup(form: Form) extends MarkupString {
  def toXml = form.toXml
  override def toString = new FormCell(form).text
}

private[specs2]
object Forms extends Forms