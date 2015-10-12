package org.specs2
package reporter

import foldm._, stream._, FoldProcessM._, FoldableProcessM._
import scalaz.concurrent.Task
import control._
import specification.core._

/**
 * A Printer is essentially defined by a FoldM sink that:
 *
 *  - can run a Process[Task, Fragment]
 *  - uses a scalaz-stream Sink for side-effects and
 *  - accumulates state for final reporting
 *
 *  See TextPrinter for an example of such a Printer
 */
trait Printer {
  def prepare(env: Env, specifications: List[SpecStructure]): Action[Unit]
  def finalize(env: Env, specifications: List[SpecStructure]): Action[Unit]

  def sink(env: Env, spec: SpecStructure): SinkTask[Fragment]

  /** convenience method to print a SpecStructure using the printer's Fold */
  def print(env: Env): SpecStructure => Task[Unit] = { spec: SpecStructure =>
    sink(env, spec).run[ProcessTask](spec.contents)
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

