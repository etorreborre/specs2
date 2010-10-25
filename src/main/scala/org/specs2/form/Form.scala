package org.specs2
package form
import matcher._
import execute._
import StandardResults._
import scalaz.{ NonEmptyList, Scalaz }
import Scalaz.{ nel1 }
import collection.Listx._

/**
 * A Form is a container for Rows (@see Row) where each row contain some Cell (@see Cell).
 * It has an optional title and possibly no rows.
 * 
 * A Form can be executed by executing each row and collecting the results. 
 */
case class Form(val title: Option[String] = None, val rows: List[Row] = (Nil: List[Row])) extends Executable with Text {

  /** add a new Row, with at least one Cell */
  def tr(c1: Cell, cs: Cell*): Form = {
    new Form(title, this.rows :+ Row.tr(c1, cs:_*))
  }
  /** add the rows of a form */
  def tr(f: Form): Form = {
    val oldRowsAndTitle = f.title.map(t => tr(new TextCell(t))).getOrElse(this).rows
    new Form(title, oldRowsAndTitle ++ f.rows)
  }
  def execute = rows.foldLeft(success: Result) { (res, cur) => res and cur.execute }
  def padText(size: Option[Int]): String = FormCell(this).padText(size)
  def header: List[Cell] = if (rows.isEmpty) Nil else rows(0).header.flatten
  lazy val allRows = title.map(t => Row.tr(TextCell(t))).toList ::: rows
  lazy val maxSizes = extendNestedList(allRows.map(_.cells)).transpose.map(l => l.map(_.text.size).max[Int])

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
case class Row(private val cellList: NonEmptyList[Cell]) extends Executable with Text {
  def padText(size: Option[Int]) = cells.map(_.padText(size)).mkString("| ", " | ", " |")
  def padText(maxSizes: List[Int]) = {
    def pad(cells: List[Cell], sizes: List[Int], result: List[String]): List[String] = {
      cells match {
        case Nil => result
        case c :: Nil => (result :+ c.padText(Some(sizes.sum + (sizes.size - 1)*3))).toList
        case c :: rest => sizes match {
          case Nil => (result :+ c.text).toList
          case s :: Nil => pad(rest, Nil, (result :+ c.padText(Some(s))).toList)
          case s :: ss => pad(rest, ss, (result :+ c.padText(Some(s))).toList)
        }
      }
    }
    pad(cells, maxSizes, Nil).mkString("| ", " | ", " |")
  }

  def execute = cells.foldLeft(success: Result) { (res, cur) => res and cur.execute }
  def cells = cellList.list
  def header = cells.map(_.header)
  def setSuccess = new Row(cellList.map(_.setSuccess))
  def setFailure = new Row(cellList.map(_.setFailure))
  
  override def equals(a: Any) = a match {
    case Row(c) => cells == c.list
    case other => false
  }
  override def hashCode = cells.map(_.hashCode).sum
}
/**
 * Companion object of a Row to create a Row with at least one cell
 */
case object Row {
  def tr(c1: Cell, cs: Cell*) = Row(nel1(c1, cs:_*))
}