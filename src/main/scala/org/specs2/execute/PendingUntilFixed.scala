package org.specs2
package execute

import control.Exceptions._
import text.Quote._

/**
 * This function allows to mark the body of an example as pending until it is fixed.
 *
 * If the result becomes a success then it is reported as a Failure so that the user thinks of
 * removing the marker
 */
trait PendingUntilFixed {

  implicit def toPendingUntilFixed[T <% Result](t: =>T) = new PendingUntilFixed(t)

  class PendingUntilFixed[T](t: =>T)(implicit toResult: T => Result) {
    /** @return Pending unless the result is a success */
    def pendingUntilFixed: Result = pendingUntilFixed("")
    /** @return Pending unless the result is a success */
    def pendingUntilFixed(m: String = ""): Result = tryOrElse(toResult(t))(Error("")) match {
      case s @ Success(_) => Failure(m.prefix(". ", "Fixed now, you should remove the 'pendingUntilFixed' marker"))
      case other          => Pending(m.prefix(". ", "Pending until fixed"))
    }
  }
}
object PendingUntilFixed extends PendingUntilFixed



