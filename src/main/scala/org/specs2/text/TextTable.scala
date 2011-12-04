package org.specs2
package text
import collection.Seqx._

/**
 * Textual representation of a table with a table and some lines.
 *
 * It is expected that the size of the header and all the lines are the same.
 *
 * The main purpose of this class is to display equal-length cells on each column
 */
case class TextTable(header: Seq[String], lines: Seq[Seq[String]], separator: String = "|") {

  /** show the table with equal-length cells */
  def show = {
    val maxByColumn = maximumsByColumn(Seq(header) ++ lines)
    formatWithMaxSize(Seq(header) ++ lines, maxByColumn).mkString("\n")
  }

  /**
   * @return a seq of lines where each cell is right-padded with the maximum size of the column cells
   */
  private def formatWithMaxSize(lines: Seq[Seq[String]], maximums: Seq[Int]): Seq[String] =
    lines.map(line => formatLineWithMaxSize(line, maximums))

  /**
   * @return a line where each cell is right-padded with the maximum size of the corresponding column
   */
  private def formatLineWithMaxSize(line: Seq[String], maximums: Seq[Int]): String =
    line.zip(maximums).map { case (cell, max) => cell.padTo(max, " ").mkString }.mkString(" "+separator+" ")

  /** @return the seq of maximum size of each column */
  private def maximumsByColumn(lines: Seq[Seq[String]]): Seq[Int] =
    transpose(lines).map(column => column.map(_.size).max)

}

object TextTable {
  def apply(header: Seq[String], lines: Seq[String]*): TextTable = new TextTable(header, lines)
}