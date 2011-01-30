package org.specs2
package specification

import control.Exceptions._
import form._
import text._
import NotNullStrings._

/**
 * Allow a Form to be inserted among Fragments as a Text Fragment
 * Allow a Form to be used as an example body and return a Result automatically
 */
trait Forms extends FormsBuilder with DecoratedProperties {
  implicit def formsAreExamples(aForm: =>Form): Example = {
    lazy val form = tryOr(aForm) { (e: Exception) =>
      Form("Initialisation error").tr(PropCell(Prop("", e.getMessage.notNull, (s: String, t: String) => execute.Error(e))("message")))
    }
    new Example(FormMarkup(form), () => form.execute) {
      override def matches(s: String) = true
    }
  }
  implicit def formsHoldersAreExamples(f: =>{ def form: Form }): Example = formsAreExamples(f.form)
}
object Forms extends Forms

/**
 * The FormMarkup embeds the description of a form as text or as Xml
 */
class FormMarkup(val form: Form) extends MarkupString {
  def toXml = form.toXml
  override def toString = new FormCell(form).text
}
object FormMarkup {
  def unapply(f: FormMarkup): Option[Form] = Some(f.form)
  def apply(f: =>Form) = new FormMarkup(f)
}
