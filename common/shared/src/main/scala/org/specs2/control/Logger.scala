package org.specs2
package control

import fp._, syntax._

trait Logger {
  def logThrowable(t: Throwable, verbose: Boolean = false): Operation[Unit]
  def warn(message: String, doIt: Boolean = true): Operation[Unit]
  def info(message: String, doIt: Boolean = true): Operation[Unit]
}

case class ConsoleLogger() extends Logger {
  def logThrowable(t: Throwable, verbose: Boolean = false): Operation[Unit] =
    Operation.delayed(println("[ERROR] "+t.getMessage)) >>
    (if (verbose) Operation.delayed(t.printStackTrace) else Operation.unit)

  def warn(message: String, doIt: Boolean = true): Operation[Unit] =
    if (doIt) Operation.delayed((println("[WARN] "+message)))
    else Operation.unit

  def info(message: String, doIt: Boolean = true): Operation[Unit] =
    if (doIt) Operation.delayed((println("[INFO] "+message)))
    else Operation.unit

}
