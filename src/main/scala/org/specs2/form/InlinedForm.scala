package org.specs2
package form

import main.Arguments
import execute.Result
import text.Trim._

/**
 * This Form overrides the toXml and text methods so that it appears seamlessly included in another Form.
 */
class InlinedForm(title: Option[String] = None, rows: Seq[Row] = Vector(), result: Option[Result] = None) extends Form(title, rows, result) {
  /** @return an xml description of this form */
  override def toXml(implicit args: Arguments = Arguments()) = <div>{Form.titleAndRows(this)(args) ++ Form.formStacktraces(this)(args) }</div>
  /** @return an xml description of this form, to be embedded in a Cell */
  override def toCellXml(implicit args: Arguments = Arguments()) = toXml(args)

  override protected def newForm(title: Option[String] = None, rows: Seq[Row] = Vector(), result: Option[Result] = None) =
    new InlinedForm(title, rows, result)

  override def text: String = super.text.removeStart("| ").removeEnd(" |")
}