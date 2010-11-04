package org.specs2
package form

import collection.Listx._
import execute._
import StandardResults._

/**
 * A Form is a container for Rows (@see Row) where each row contain some Cell (@see Cell).
 * It has an optional title and possibly no rows.
 * 
 * A Form can be executed by executing each row and collecting the results. 
 */
case class Form(val title: Option[String] = None, val rows: List[Row] = (Nil: List[Row])) extends Executable with Text {

  /** @return the labels of all rows to build a header for the form */
  def header: List[Cell] = if (rows.isEmpty) Nil else rows(0).header.flatten
  
  /** @return all rows, including the header */
  lazy val allRows = title.map(t => Row.tr(TextCell(t))).toList ::: rows

  /** @return the maximum cell size, column by column */
  lazy val maxSizes = allRows.map(_.cells).safeTranspose.map(l => l.map(_.text.size).max[Int])

  /** @return a Form where every Row is executed with a Success */
  def setSuccess = new Form(title, rows.map(_.setSuccess))
  /** @return a Form where every Row is executed with a Failure */
  def setFailure = new Form(title, rows.map(_.setFailure))

  /** add a new Row, with at least one Cell */
  def tr(c1: Cell, cs: Cell*): Form = {
    new Form(title, this.rows :+ Row.tr(c1, cs:_*))
  }
  /** add the rows of a form */
  def tr(f: Form): Form = {
    val oldRowsAndTitle = f.title.map(t => tr(new TextCell(t))).getOrElse(this).rows
    new Form(title, oldRowsAndTitle ++ f.rows)
  }
  
  /** 
   * execute all rows
   * @return a logical and on all results 
   */
  def execute = rows.foldLeft(success: Result) { (res, cur) => res and cur.execute }

  /** @return the printed form with a padding space size to use for each cell */
  def padText(size: Option[Int]): String = FormCell(this).padText(size)
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
