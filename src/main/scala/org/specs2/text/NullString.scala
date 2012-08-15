package org.specs2
package text

import control.Exceptions._

/**
 * Utility methods to replace a null String with "null"
 *
 * They also make sure that the toString or mkString methods don't throw exceptions when being evaluated
 */
private[specs2]
trait NotNullStrings {

  implicit def anyToNotNull(a: Any) = new NotNullAny(a)
  class NotNullAny(a: Any) {
    def  notNull: String = {
      if (a == null) "null"
      else {
        a match {
          case ar: Array[_]           => ar.notNullMkString(", ", "Array(", ")")
          case it: TraversableOnce[_] => it.notNullMkString(", ")
          case _                      => evaluate(a)
        }
      }
    }
  }

  private def evaluate(value: =>Any, msg: String = "Exception when evaluating toString: ") = {
    val string = catchAllOr(value.toString) { (t: Throwable) => msg + t.getMessage }
    if (string == null) "null"
    else                string
  }

  trait NotNullMkString {
    def notNullMkString(sep: String, start: String = "", end: String = ""): String
  }
  implicit def arrayToNotNull[T](a: Array[T]): NotNullMkString = if (a == null) new NullMkString else new NotNullTraversableOnce(a.toSeq)

  class NullMkString extends NotNullMkString {
    def notNullMkString(sep: String, start: String = "", end: String = ""): String = "null"
  }

  implicit def traversableOnceToNotNull[T](a: =>TraversableOnce[T]): NotNullTraversableOnce[T] = new NotNullTraversableOnce(a)
  class NotNullTraversableOnce[T](a: =>TraversableOnce[T]) extends NotNullMkString {
    def notNullMkString(sep: String, start: String = "", end: String = ""): String = {
      if (a == null) "null"
      else evaluate(catchAllOrElse(a.mkString(start, sep, end))(evaluate(a.toString)))
    }
  }


}
private[specs2]
object NotNullStrings extends NotNullStrings