package org.specs2
package control

import io._
import fp._, syntax._

trait Logger {
  def warn(message: String, doIt: Boolean = true): Operation[Unit]
  def info(message: String, doIt: Boolean = true): Operation[Unit]
  def exception(t: Throwable, doIt: Boolean = true): Operation[Unit]

  // derived operations
  def warnAndFail[A](warnMessage: String, failureMessage: String, doIt: Boolean = true): Operation[A] =
    warn(warnMessage, doIt) >> Operation.fail[A](failureMessage)
}

case class ConsoleLogger() extends Logger {

  def exception(t: Throwable, verbose: Boolean = false): Operation[Unit] =
    Operation.delayed(println("[ERROR] "+t.getMessage)) >>
    (if (verbose) Operation.delayed(t.printStackTrace) else Operation.unit)

  def warn(message: String, doIt: Boolean = true): Operation[Unit] =
    if (doIt) Operation.delayed((println("[WARN] "+message)))
    else Operation.unit

  def info(message: String, doIt: Boolean = true): Operation[Unit] =
    if (doIt) Operation.delayed((println("[INFO] "+message)))
    else Operation.unit

}

object NoLogger extends Logger {
  def warn(message: String, doIt: Boolean = true): Operation[Unit] = Operation.unit
  def info(message: String, doIt: Boolean = true): Operation[Unit] = Operation.unit
  def exception(t: Throwable, doIt: Boolean = true): Operation[Unit] = Operation.unit
}

case class StringOutputLogger(output: StringOutput) extends Logger {
  def warn(message: String, doIt: Boolean = true): Operation[Unit] =
    Operation.ok(if (doIt) output.printf(message))

  def info(message: String, doIt: Boolean = true): Operation[Unit] =
    Operation.ok(if (doIt) output.printf(message))

  def exception(t: Throwable, doIt: Boolean = true): Operation[Unit] =
    Operation.ok(if (doIt) output.printf(t.getMessage))
}
