package org.specs2
package text
import control.Exceptions._

/**
 * Utility method to replace a null String with ""
 */
private[specs2]
trait NotNullStrings {
  implicit def anyToNotNull(a: Any) = new NotNullAny(a)
  class NotNullAny(a: Any) {
    def notNull: String = {
      if (a == null) "null"
      else {
        val string = tryOr(a.toString) { (e: Exception) => "Exception when evaluating toString "+e.getMessage }
        if (string == null) "null"
        else                string
      }
    }
  }
}
private[specs2]
object NotNullStrings extends NotNullStrings