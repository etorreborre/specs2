package org.specs2
package execute

import text.Quote._

/**
 * This function allows to mark the body of an example as pending until it is fixed.
 *
 * If the result becomes a success then it is reported as a Failure so that the user thinks of
 * removing the marker
 */
trait PendingUntilFixed {

  implicit def toPendingUntilFixed[T : AsResult](t: =>T) = new PendingUntilFixed(t)

  class PendingUntilFixed[T : AsResult](t: =>T) {
    /** @return Pending unless the result is a success */
    def pendingUntilFixed: Result = pendingUntilFixed("")
    /** @return Pending unless the result is a success */
    def pendingUntilFixed(m: String = ""): Result = ResultExecution.execute(AsResult(t)) match {
      case s @ Success(_,_) => Failure(m.prefix(". ", "Fixed now, you should remove the 'pendingUntilFixed' marker"))
      case other            => Pending(m.prefix(". ", "Pending until fixed"))
    }
  }
}

/**
 * use this trait to remove the pending until fixed implicit conversion
 */
trait NoPendingUntilFixed extends PendingUntilFixed {
  override def toPendingUntilFixed[T : AsResult](t: =>T) = super.toPendingUntilFixed(t)
}

object PendingUntilFixed extends PendingUntilFixed



