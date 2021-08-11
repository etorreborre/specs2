package org.specs2
package execute

import text.Quote.*
import scala.util.NotGiven
import specification.core.*

/**
 * This function allows to mark the body of an example as pending until it is fixed.
 *
 * If the result becomes a success then it is reported as a Failure so that the user thinks of
 * removing the marker
 */
trait PendingUntilFixed:
  outer =>

  /** @return Pending unless the result of the execution  is a success */
  def pendingUntilFixed[T : AsExecution](m: String)(t: =>T): Execution =
    AsExecution[T].execute(t).mapFinalResult {
      case s @ Success(_,_) =>
        Failure(m.prefix(". ", "Fixed now, you should remove the 'pendingUntilFixed' marker"))

      case other =>
        Pending(m.prefix(". ", "Pending until fixed"))
    }

  // postfix methods
  extension [T : AsExecution](t: =>T)(using not: NotGiven[NoPendingUntilFixed])
    /** @return Pending unless the result is a success */
    def pendingUntilFixed: Execution =
      outer.pendingUntilFixed("")(t)

    /** @return Pending unless the result is a success */
    def pendingUntilFixed(m: String): Execution =
      outer.pendingUntilFixed(m)(t)

/**
 * use this trait to remove the pending until fixed implicit conversion
 */
trait NoPendingUntilFixed extends PendingUntilFixed:
  given NoPendingUntilFixed = ???

object PendingUntilFixed extends PendingUntilFixed
