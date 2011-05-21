package org.specs2
package form

import org.specs2.internal.scalaz.{ NonEmptyList, Scalaz }
import Scalaz.{ nel }
import execute._
import StandardResults._

/**
 * A Row is a non-empty list of Cells
 * 
 * A Row can be executed by executing each Cell and collecting the results.
 */
case class Row(private val cellList: NonEmptyList[Cell]) extends Executable {
  /** @return all the cells */
  def cells = cellList.list
  
  /** @return a Row where every cell is executed with a Success */
  def setSuccess = new Row(cellList.map(_.setSuccess))
  /** @return a Row where every cell is executed with a Failure */
  def setFailure = new Row(cellList.map(_.setFailure))
  
  /** 
   * execute all cells
   * @return a logical `and` on all results
   */
  def execute = cells.foldLeft(success: Result) { (res, cur) => res and cur.execute }
  /**
   * execute the row
   * @return the Row with executed cells
   */
  def executeRow = Row(cellList.map(_.executeCell))

  /** @return print the row with a padding space size to use for each cell, given cell by cell */
  def text(maxSizes: List[Int]) = {
    def pad(cells: List[Cell], sizes: List[Int], result: List[String]): List[String] = {
      cells match {
        case Nil => result
        case c :: Nil => (result :+ c.text.padTo(sizes.sum + (sizes.size - 1)*3, ' ')).toList
        case c :: rest => sizes match {
          case Nil => (result :+ c.text).toList
          case s :: Nil => pad(rest, Nil, (result :+ c.text.padTo(s, ' ')).toList)
          case s :: ss => pad(rest, ss, (result :+ c.text.padTo(s, ' ')).toList)
        }
      }
    }
    pad(cells, maxSizes, Nil).mkString("| ", " | ", " |")
  }
  
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
  /**
   * create a row from cells
   */
  def tr(c1: Cell, cs: Cell*) = Row(nel(c1, cs:_*))
}
