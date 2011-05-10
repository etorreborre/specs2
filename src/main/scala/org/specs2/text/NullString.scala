package org.specs2
package text

/**
 * Utility method to replace a null String with ""
 */
private[specs2]
trait NotNullStrings {
  implicit def anyToNotNull(a: Any) = new NotNullAny(a)
  class NotNullAny(a: Any) {
    def notNull = if (a == null) "null" else a.toString
  }
}
private[specs2]
object NotNullStrings extends NotNullStrings