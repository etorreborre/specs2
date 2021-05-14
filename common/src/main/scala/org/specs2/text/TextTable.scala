package org.specs2
package text

import collection.Seqx.*

/**
 * Textual representation of a table with a table and some lines.
 *
 * It is expected that the size of the header and all the lines are the same.
 *
 * The main purpose of this class is to:
 *   - display equal-length cells on each column
 *   - allow line breaks in cells for each line
 */
case class TextTable(header: Seq[String], lines: Seq[Seq[String]], separator: String = "|"):

  /** show the table with equal-length cells */
  def show =
    val heightFormatted: Seq[Seq[String]] = (header +: lines).flatMap(formatHeight)
    val maxByColumn = maximumsByColumn(heightFormatted)
    formatWithMaxSize(heightFormatted, maxByColumn).mkString("\n")

  /**
   * Format each line cell by splitting them along new lines and adding
   * padding with additional rows
   */
  def formatHeight(line: Seq[String]): Seq[Seq[String]] =
    val cells = line.map(_.split("\n").toSeq)
    val maxHeight = cells.map(_.size).max
    val heightPadded = cells.map(c => c ++ List.fill(maxHeight - c.size)(""))
    transpose(heightPadded)

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
    transpose(lines).map(column => column.map(_.length).max)


object TextTable:
  def apply(header: Seq[String], lines: Seq[String]*): TextTable =
    new TextTable(header, lines)
