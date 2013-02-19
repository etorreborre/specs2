package org.specs2
package text

import control.Exceptions._
import Quote._

/**
 * Utility methods to replace a null String with "null"
 *
 * They also make sure that the toString or mkString methods don't throw exceptions when being evaluated
 */
private[specs2]
trait NotNullStrings {

  /**
   * @return the string evaluation of an object
   *
   * - if it is null => null
   * - if this is an Array or a collection => it tries the toString method of the collection or evaluate each element separately (because they could be null too)
   * - if this is another type of object   => calls the toString method
   */
  implicit def anyToNotNull(a: Any) = new NotNullAny(a)
  class NotNullAny(a: Any) {
    def  notNull: String = {
      if (a == null) "null"
      else {
        a match {
          case ar: Array[_]           => ar.notNullMkString(", ", "Array(", ")")
          case it: TraversableOnce[_] => it.notNullMkStringWith(addQuotes = false)
          case _                      => evaluate(a)
        }
      }
    }

    /**
     * @return the string evaluation of an object + its class name
     *
     * - if it is null => null
     * - if this is an Array or a collection => it prints the class name of the full collection if each element has the same type: List[Int] for example
     *                                          other it prints each element with its class name
     * - if this is another type of object   => calls the toString method + getClass.getName
     */
    def  notNullWithClass: String = {
      if (a == null) "null"
      else {
        def sameElementTypes(ts: TraversableOnce[_]) =
          ts.nonEmpty && (ts.toSeq.collect { case t if !(t == null) => t.getClass.getName }.size == ts.size)
        tryOrElse {
          a match {
            case ar: Array[_]           =>
              if (sameElementTypes(ar)) ar.map(a => quote(a.notNull)).mkString(", ", "Array(", "): Array["+ar(0).getClass.getName+"]")
              else                      ar.map(_.notNullWithClass).mkString(", ", "Array(", ")")
            case it: TraversableOnce[_] =>
              if (sameElementTypes(it)) it.toSeq.notNullMkStringWith(addQuotes = true)+": "+it.getClass.getName+"["+it.toSeq(0).getClass.getName+"]"
              else                      it.toSeq.map(_.notNullWithClass)+": "+it.getClass.getName
            case _                      => evaluate(a)+": "+a.getClass.getName
          }
        }(evaluate(a)+": "+a.getClass.getName) // in case the collection throws an exception during its traversal
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
  class NotNullTraversableOnce[T](values: =>TraversableOnce[T]) extends NotNullMkString {
    def notNullMkString(sep: String, start: String = "", end: String = ""): String = {
      if (values == null) "null"
      else                evaluate(catchAllOrElse(values.mkString(start, sep, end))(evaluate(values.toString)))
    }
    def notNullMkStringWith(addQuotes: Boolean = false): String = {
      def traversableWithQuotedElements = values.toSeq.map(v => quote(evaluate(v), addQuotes)).toString
      def quotedTraversable             = quote(evaluate(values.toString))

      if (values == null) quote("null", addQuotes)
      else if (addQuotes) evaluate(catchAllOrElse(traversableWithQuotedElements)(quotedTraversable))
      else                evaluate(catchAllOrElse(evaluate(values.toString))(values.toSeq.map(v => evaluate(v)).toString))
    }
  }

}
private[specs2]
object NotNullStrings extends NotNullStrings