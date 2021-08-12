package org.specs2
package form

import execute.*
import StandardResults.*
import ResultLogicalCombinators.*

/** A Row is a non-empty list of Cells
  *
  * A Row can be executed by executing each Cell and collecting the results.
  */
case class Row(private val cellList: List[Cell]) extends Executable:
  /** @return all the cells */
  def cells = cellList

  /** @return a Row where every cell is executed with a Success */
  def setSuccess = setResult(success)

  /** @return a Row where every cell is executed with a Failure */
  def setFailure = setResult(failure)

  /** @return a Row where every cell is executed with a Skipped */
  def setSkipped = setResult(skipped)

  /** @return a Row where every cell is executed with a Pending */
  def setPending = setResult(pending)

  /** @return a Row where every cell is executed with a specified Result */
  def setResult(r: Result) = new Row(cellList.map(_.setResult(r)))

  /** execute all cells
    * @return
    *   a logical and on all results
    */
  def execute: Result =
    cellList.foldLeft(success: Result) { (res, cur) => res and cur.execute }

  /** execute the row
    * @return
    *   a new Row with executed cells
    */
  def executeRow: Row =
    Row(cellList.map(_.executeCell))

  /** @return print the row with a padding space size to use for each cell, given cell by cell */
  def text(maxSizes: Seq[Int]): String =
    def pad(cells: List[Cell], sizes: List[Int], result: List[String]): List[String] =
      cells match
        case List()  => result
        case List(c) => result :+ c.text.padTo(sizes.sum + (sizes.size - 1) * 3, ' ')
        case c :: rest =>
          sizes match
            case List()  => result :+ c.text
            case List(s) => pad(rest, Nil, result :+ c.text.padTo(s, ' '))
            case s :: ss => pad(rest, ss, result :+ c.text.padTo(s, ' '))
    pad(cells.toList, maxSizes.toList, Nil).mkString("| ", " | ", " |")

  /** append a new Cell */
  def add(cell: Cell) =
    copy(cellList = cellList :+ cell)

  override def equals(a: Any) = a.asInstanceOf[Matchable] match
    case Row(c) => cells == c
    case other  => false
  override def hashCode = cells.map(_.hashCode).sum

/** Companion object of a Row to create a Row with at least one cell
  */
case object Row:
  /** create a row from cells
    */
  def tr(c1: Cell, cs: Cell*) = Row(c1 +: cs.toList)

  /** create a row from cells
    */
  def tr(cs: Seq[Cell]) = Row(cs.toList)
