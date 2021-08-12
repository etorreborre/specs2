package org.specs2
package form

import language.adhocExtensions
import scala.xml.NodeSeq
import main.Arguments
import execute.Result
import text.Trim.*

/** This Form overrides the toXml and text methods so that it appears seamlessly included in another Form.
  */
class InlinedForm(title: Option[String] = None, rows: Seq[Row] = Vector(), result: Option[Result] = None)
    extends Form(title, rows, result):

  /** @return an xml description of this form */
  override def toXml(using args: Arguments = Arguments()): NodeSeq =
    <div>{Form.titleAndRows(this) ++ Form.formStacktraces(this)}</div>

  /** @return an xml description of this form, to be embedded in a Cell */
  override def toCellXml(using args: Arguments = Arguments()): NodeSeq =
    toXml(using args)

  override protected def newForm(
      title: Option[String] = None,
      rows: Seq[Row] = Vector(),
      result: Option[Result] = None
  ) =
    new InlinedForm(title, rows, result)

  override def text: String =
    super.text.removeStart("| ").removeEnd(" |")
