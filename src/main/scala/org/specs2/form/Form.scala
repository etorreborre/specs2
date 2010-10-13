package org.specs2
package form
import execute._
import matcher._
import StandardResults._
import scalaz.{ NonEmptyList, Scalaz }
import Scalaz.{ nel1 }

/**
 * A Form is a container for Rows (@see Row) where each row contain some Cell (@see Cell).
 * It has an optional title and possibly no rows.
 * 
 * A Form can be executed by executing each row and collecting the results. 
 */
case class Form(val title: Option[String] = None, val rows: List[Row] = (Nil: List[Row])) extends Executable with Text {

  /** add a new Row, with at least one Cell */
  def tr(c1: Cell, cs: Cell*) = {
    new Form(title, this.rows :+ Row.tr(c1, cs:_*))
  }
  def execute = rows.foldLeft(success: Result) { (res, cur) => res and cur.execute }
  def text = FormCell(this).text
  def header: List[Cell] = if (rows.isEmpty) Nil else rows(0).header.flatten
  def setSuccess = new Form(title, rows.map(_.setSuccess))
  def setFailure = new Form(title, rows.map(_.setFailure))
}
/**
 * Companion object of a Form to create:
 *   * an empty Form
 *   * a Form with no rows but a title
 *   * a Form with no title but one row
 *
 */
case object Form {
  def apply() = new Form(None, Nil)
  def apply(title: String) = new Form(Some(title), Nil)
  def tr(c1: Cell, c: Cell*) = new Form().tr(c1, c:_*)
}
/**
 * A Row is a non-empty list of Cells
 * 
 * A Row can be executed by executing each Cell and collecting the results. 
 */
case class Row(cells: NonEmptyList[Cell]) extends Executable with Text {
  def text = cells.map(_.text).list.mkString("| ", " | ", " |")
  def execute = cells.list.foldLeft(success: Result) { (res, cur) => res and cur.execute }
  def header = cells.list.map(_.header)
  def setSuccess = new Row(cells.map(_.setSuccess))
  def setFailure = new Row(cells.map(_.setFailure))
  
  override def equals(a: Any) = a match {
    case Row(c) => cells.list == c.list
    case other => false
  }
  override def hashCode = cells.list.map(_.hashCode).sum
}
/**
 * Companion object of a Row to create a Row with at least one cell
 */
case object Row {
  def tr(c1: Cell, cs: Cell*) = Row(nel1(c1, cs:_*))
}

/**
 * Base type for a Cell
 *
 * A Cell can be transformed to a text representation
 * and it can also be executed.
 */
trait Cell extends Text with Executable {
  def header = List(this)
  def setSuccess: Cell
  def setFailure: Cell
}
/**
 * Base type for anything returning some text
 */
trait Text { def text: String }

/**
 * Simple Cell embedding an arbitrary String
 */
case class TextCell(s: String, result: Result = skipped) extends Cell {
  def text = s.toString
  def execute = result
  def setSuccess = TextCell(s, success)
  def setFailure = TextCell(s, failure)
}
/**
 * Cell embedding a Field
 */
case class FieldCell(f: Field[_], result: Result = skipped) extends Cell {
  def text = f.toString
  def execute = result
  override def header = List(TextCell(f.label))
  def setSuccess = FieldCell(f, success)
  def setFailure = FieldCell(f, failure)
}
/**
 * Cell embedding a Field
 */
case class PropCell(p: Prop[_,_], result: Option[Result] = None) extends Cell {
  def text = p.expected.getOrElse("_").toString
  def execute = result.getOrElse(p.execute)
  override def header = List(TextCell(p.label))
  def setSuccess = PropCell(p, Some(success))
  def setFailure = PropCell(p, Some(failure))
}
/**
 * Cell embedding a Form
 */
case class FormCell(form: Form) extends Cell {
  def text: String = {
    (form.title.map("| "+_+" |").getOrElse("") :: form.rows.map(_.text)).
         filterNot(_.isEmpty).mkString("\n")
  }
  def execute = form.execute
  override def header = form.header
  def setSuccess = FormCell(form.setSuccess)
  def setFailure = FormCell(form.setFailure)
}

