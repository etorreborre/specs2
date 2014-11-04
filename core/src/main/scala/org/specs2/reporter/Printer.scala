package org.specs2
package reporter

import scalaz.concurrent.Task
import data.Fold
import org.specs2.control._
import specification.core._
/**
 * A Printer is essentially defined by a Fold that:
 *
 *  - can run a Process[Task, Fragment]
 *  - uses a Sink for side-effects and
 *  - accumulates state for final reporting
 */
trait Printer {
  def prepare(env: Env, specifications: List[SpecificationStructure]): Action[Unit]
  def finalize(env: Env, specifications: List[SpecificationStructure]): Action[Unit]

  def fold(env: Env, spec: SpecStructure): Fold[Fragment]

  /** convenience method to print a SpecStructure using the printer's Fold */
  def print(env: Env): SpecStructure => Task[Unit] = { spec: SpecStructure =>
    Fold.runFold(spec.contents, fold(env, spec))
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

