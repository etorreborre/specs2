package org.specs2
package text

private[specs2]
trait NotNullStrings {
  implicit def stringToNotNull(s: String) = new NotNullString(s)
  class NotNullString(s: String) {
    def notNull = if (s == null) "null" else s
  }
}
private[specs2]
object NotNullStrings extends NotNullStrings