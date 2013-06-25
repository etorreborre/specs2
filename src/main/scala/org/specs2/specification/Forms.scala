package org.specs2
package specification

import control.Exceptions._
import execute.Success
import form._
import text._
import NotNullStrings._

/**
 * Allow a Form to be inserted among Fragments as a Text Fragment
 * Allow a Form to be used as an example body and return a Result automatically
 */
trait Forms extends FormsBuilder with DecoratedProperties {
  implicit def formsAreExamples(aForm: =>Form): Example = {
    lazy val form = tryOr(aForm.executeForm) { (e: Exception) =>
      Form("Initialisation error").tr(PropCell(Prop("", e.getMessage.notNull, (s: String, t: String) => execute.Error(e))("message")))
    }
    new Example(FormFormattedString.create(form), () => form.result.getOrElse(Success(""))) {
      override def matches(s: String) = true
    }
  }
  implicit def formsHoldersAreExamples(f: =>{ def form: Form }): Example = formsAreExamples(f.form)
}
object Forms extends Forms

/**
 * The FormFormattedString embeds the description of a form as text or as Xml
 */
case class FormFormattedString(f: () => Form, formatting: Formatting = Formatting(),
                               isEmpty: Boolean = false,
                               display: Form => String = (ff: Form) => new FormCell(ff).text) extends FormattedString {
  type F = FormFormattedString

  lazy val form = f()

  def toXml = form.toXml
  override def raw = toString
  override def toString = display(form)
  def map(f: String => String) = copy(display = display andThen f)

  def withMarkdown = copy(formatting = formatting.copy(markdown = true))
  def withFlow = copy(formatting = formatting.copy(flow = true))

  def formatWithTagNames(names: Seq[String]) = copy(formatting = formatting.fromTagNames(names: Seq[String]))
}
object FormFormattedString {
  def create(f: =>Form) = new FormFormattedString(() => f)
}
