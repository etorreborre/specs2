package org.specs2
package execute

import text.Quote._
import org.specs2.specification.core._

/**
 * This function allows to mark the body of an example as pending until it is fixed.
 *
 * If the result becomes a success then it is reported as a Failure so that the user thinks of
 * removing the marker
 */
trait PendingUntilFixed { outer =>

  /** @return Pending unless the result of the execution is a success */
  def pendingUntilFixed[T : AsExecution](t: =>T): Execution =
    pendingUntilFixed("")(t)

  /** @return Pending unless the result of the execution is a success */
  def pendingUntilFixed[T : AsExecution](m: String = "")(t: =>T): Execution =
    AsExecution[T].execute(t).mapFinalResult {
    case s @ Success(_,_) => Failure(m.prefix(". ", "Fixed now, you should remove the 'pendingUntilFixed' marker"))
    case other            => Pending(m.prefix(". ", "Pending until fixed"))
  }

  // postfix methods
  implicit def toPendingUntilFixed[T : AsExecution](t: =>T): PendingUntilFixed[T] =
    new PendingUntilFixed(t)

  class PendingUntilFixed[T : AsExecution](t: =>T) {
    def pendingUntilFixed: Execution =
      outer.pendingUntilFixed("")(t)

    def pendingUntilFixed(m: String = ""): Execution =
      outer.pendingUntilFixed(m)(t)
  }

}

/**
 * use this trait to remove the pending until fixed implicit conversion
 */
trait NoPendingUntilFixed extends PendingUntilFixed {
  override def toPendingUntilFixed[T : AsExecution](t: =>T) = super.toPendingUntilFixed(t)
}

object PendingUntilFixed extends PendingUntilFixed
