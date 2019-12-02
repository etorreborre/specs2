package org.specs2
package reporter

import control._
import origami._
import specification.core._

/**
 * A Printer is essentially defined by a FoldM sink that:
 *
 *  - can run a AsyncStream[Fragment]
 *  - uses an AsyncSink for side-effects and
 *  - accumulates state for final reporting
 *
 *  See TextPrinter for an example of such a Printer
 */
trait Printer {
  def prepare(specifications: List[SpecStructure]): Action[Unit]
  def finalize(specifications: List[SpecStructure]): Action[Unit]

  def sink(spec: SpecStructure): AsyncSink[Fragment]

  /** convenience method to print a SpecStructure using the printer's Fold */
  def print(spec: SpecStructure): Action[Unit] = {
    val printSink = sink(spec)
    spec.contents.fold(printSink.start, printSink.fold, printSink.end)
  }
}

/**
 * specs2 built-in printers and creation methods based on the command line arguments
 */
object Printer {
  val CONSOLE  = PrinterName("console")
  val HTML     = PrinterName("html")
  val JUNIT    = PrinterName("junit")
  val MARKDOWN = PrinterName("markdown")
  val JUNITXML = PrinterName("junitxml")
  val PRINTER  = PrinterName("printer")
  val NOTIFIER = PrinterName("notifier")

  val printerNames = Seq(CONSOLE, HTML, JUNIT, JUNITXML, MARKDOWN, PRINTER, NOTIFIER)

  case class PrinterName(name: String) extends AnyVal
}
