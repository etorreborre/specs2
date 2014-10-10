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
  implicit def anyToNotNull(a: Any): NotNullAny = new NotNullAny(a)
  class NotNullAny(a: Any) {
    def notNull: String = {
      if (a == null) "null"
      else {
        a match {
          case ar: Array[_]           => ar.notNullMkString(", ", "Array(", ")")
          case map: Map[_,_]          => new NotNullMap(map).notNullMkStringWith(addQuotes = false)
          case n: scala.xml.Node      => evaluate(n)
          case it: TraversableOnce[_] => it.notNullMkStringWith(addQuotes = false)
          case _                      => evaluate(a)
        }
      }
    }

    /**
     * @return the string evaluation of an object + its class name
     *
     * - if it is null => null
     * - if this is an Array or a traversable => it prints the class name of the full collection if each element has the same type: List[Int] for example
     *                                          other it prints each element with its class name (with a special case for maps)
     * - if this is another type of object   => calls the toString method + getClass.getName
     *
     * The classes of nested values are not shown unless `showAll` is true
     */
    def notNullWithClass(showAll: Boolean): String = {
      if (a == null) "null"
      else {
        def sameElementTypes(ts: TraversableOnce[_]) =
          ts.nonEmpty && (ts.toSeq.collect { case t if t != null => t.getClass.getName }.distinct.size == 1)

        def sameKeyValueTypes(map: Map[_,_]) = sameElementTypes(map.keys) && sameElementTypes(map.values)

        tryOrElse {
          a match {
            case ar: Array[_] =>
              if (!showAll && sameElementTypes(ar)) ar.map(a => quote(a.notNull)).mkString("Array(", ", ", "): Array["+ar(0).getClass.getName+"]")
              else                                  ar.map(_.notNullWithClass(showAll)).mkString("Array(", ", ", ")")
            case map: Map[_,_] =>
              if (!showAll && sameKeyValueTypes(map)) map.notNullMkStringWith(addQuotes = true)+": "+map.getClass.getName+"["+map.toSeq(0).getClass.getName+"]"
              else                                    map.map { case (k, v) => (k.notNullWithClass(showAll), v.notNullWithClass(showAll)) }+": "+map.getClass.getName
            case n: scala.xml.Node      =>    evaluate(n)+": "+n.getClass.getName
            case it: TraversableOnce[_] =>
              if (!showAll && sameElementTypes(it))   it.toSeq.notNullMkStringWith(addQuotes = true)+": "+it.getClass.getName+"["+it.toSeq(0).getClass.getName+"]"
              else                                    it.toSeq.map(_.notNullWithClass(showAll))+": "+it.getClass.getName
            case _ =>                     evaluate(a)+": "+a.getClass.getName
          }
        }(evaluate(a)+": "+a.getClass.getName) // in case the collection throws an exception during its traversal
      }
    }
    /** default case for the notNullWithClass method */
    def notNullWithClass: String = notNullWithClass(showAll = false)
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

  implicit class NotNullMap[K, V](map: =>Map[K,V]) {
    def notNullMkStringWith(addQuotes: Boolean = false): String = {
      def mapWithQuotedElements = map.map { case (k, v) => (quote(evaluate(k), addQuotes), quote(evaluate(v), addQuotes)) }.toString
      def quotedMap             = quote(evaluate(map.toString))

      if (map == null)    quote("null", addQuotes)
      else if (addQuotes) evaluate(catchAllOrElse(mapWithQuotedElements)(quotedMap))
      else                evaluate(catchAllOrElse(evaluate(map.toString))(map.map { case (k, v) => (evaluate(k), evaluate(v)) }.toString))
    }
  }

  /**
   * display the exception message only if the exception is not null
   */
  def causedBy(t: Throwable, separator: String = ": ") =
    if (t.getMessage == null) ""
    else separator+t.getMessage

  // display pairs nicely
  val notNullPair: Any => String = (a: Any) =>
    a match {
      case (k, v) => s"$k -> $v"
      case _      => a.notNull
    }
}
private[specs2]
object NotNullStrings extends NotNullStrings