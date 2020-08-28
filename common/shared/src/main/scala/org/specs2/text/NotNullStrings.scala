package org.specs2
package text

import control.Exceptions._
import Quote._

/**
 * Methods to replace a null String with "null"
 *
 * There are also methods to make sure that the toString or mkString methods don't throw exceptions when being evaluated
 */
private[specs2]
trait NotNullStrings:

  /**
   * @return the string evaluation of an object
   *
   * - if it is null => null
   * - if this is an Array or a collection => it tries the toString method of the collection or evaluate each element separately (because they could be null too)
   * - if this is another type of object   => calls the toString method
   */
  implicit def anyToNotNull(a: Any): NotNullAny = new NotNullAny(a)
  class NotNullAny(a: Any):
    def notNull: String =
      if a == null then "null"
      else
        a match
          case ar: Array[_]    => ar.notNullMkString(", ", "Array(", ")")
          case map: Map[_,_]   => new NotNullMap(map).notNullMkStringWith(addQuotes = false)
          case it: Iterable[_] => it.notNullMkStringWith(addQuotes = false)
          case _               => evaluate(a)

    /**
     * @return the string evaluation of an object + its class name
     *
     * - if it is null => null
     * - if this is an Array or an Iterable => it prints the class name of the full collection if each element has the same type: List[Int] for example
     *                                          other it prints each element with its class name (with a special case for maps)
     * - if this is another type of object   => calls the toString method + getClass.getName
     *
     * The classes of nested values are not shown unless `showAll` is true
     */
    def notNullWithClass(showAll: Boolean): String =
      if a == null then "null"
      else
        def sameElementTypes(ts: Iterable[_]) =
          ts.nonEmpty && (ts.toSeq.collect { case t if t != null => t.getClass.getName }.distinct.size == 1)

        def sameKeyValueTypes(map: Map[_,_]) = sameElementTypes(map.keys) && sameElementTypes(map.values)

        tryOrElse {
          a match
            case ar: Array[_] =>
              if !showAll && sameElementTypes(ar) then
                ar.map(a => quote(a.notNull)).mkString("Array(", ", ", "): Array["+ar(0).getClass.getName+"]")
              else
                ar.map(_.notNullWithClass(showAll)).mkString("Array(", ", ", ")")

            case map: Map[_,_] =>
              if !showAll && sameKeyValueTypes(map) then
                map.notNullMkStringWith(addQuotes = true)+": "+map.getClass.getName+"["+map.head.getClass.getName+"]"
              else
                map.map { case (k, v) => (k.notNullWithClass(showAll), v.notNullWithClass(showAll)) }.mkString("Map(", ", ", ")")+
                  ": "+map.getClass.getName

            case it: Iterable[_] =>
              if !showAll && sameElementTypes(it) then
                it.notNullMkStringWith(addQuotes = true)+": "+it.getClass.getName+"["+it.head.getClass.getName+"]"
              else
                it.map(_.notNullWithClass(showAll)).toString+": "+it.getClass.getName

            case _ => evaluate(a)+": "+a.getClass.getName
        }(evaluate(a)+": "+a.getClass.getName) // in case the collection throws an exception during its traversal
    /** default case for the notNullWithClass method */
    def notNullWithClass: String = notNullWithClass(showAll = false)

  private def evaluate(value: =>Any, msg: String = "Exception when evaluating toString: ") =
    val string = catchAllOr(value.toString) { (t: Throwable) => msg + t.getMessage }
    if string == null then "null"
    else                string

  trait NotNullMkString:
    def notNullMkString(sep: String, start: String = "", end: String = ""): String
  implicit def arrayToNotNull[T](a: Array[T]): NotNullMkString = if a == null then new NullMkString else new NotNullIterable(a.toSeq)

  class NullMkString extends NotNullMkString:
    def notNullMkString(sep: String, start: String = "", end: String = ""): String = "null"

  implicit def iterableToNotNull[T](a: =>Iterable[T]): NotNullIterable[T] = new NotNullIterable(a)
  class NotNullIterable[T](values: =>Iterable[T]) extends NotNullMkString:
    def notNullMkString(sep: String, start: String = "", end: String = ""): String =
      if values == null then
        "null"
      else
        evaluate(catchAllOrElse(values.iterator.mkString(start, sep, end))(evaluate(values.toString)))
    def notNullMkStringWith(addQuotes: Boolean = false): String =
      def iterableWithQuotedElements = values.map(v => quote(evaluate(v), addQuotes)).toString
      def quotedIterable             = quote(evaluate(values.toString))

      if values == null then quote("null", addQuotes)
      else if addQuotes then evaluate(catchAllOrElse(iterableWithQuotedElements)(quotedIterable))
      else                evaluate(catchAllOrElse(evaluate(values.toString))(values.map(v => evaluate(v)).toString))

  implicit class NotNullMap[K, V](map: =>Map[K,V]):
    def notNullMkStringWith(addQuotes: Boolean = false): String =
      def mapWithQuotedElements = map.map { case (k, v) => (quote(evaluate(k), addQuotes), quote(evaluate(v), addQuotes)) }.toString
      def quotedMap             = quote(evaluate(map.toString))

      if map == null then    quote("null", addQuotes)
      else if addQuotes then evaluate(catchAllOrElse(mapWithQuotedElements)(quotedMap))
      else                evaluate(catchAllOrElse(evaluate(map.toString))(map.map { case (k, v) => (evaluate(k), evaluate(v)) }.toString))

  // display pairs nicely
  val notNullPair: Any => String =
    case (k, v) => s"$k -> $v"
    case a      => a.notNull

private[specs2]
object NotNullStrings extends NotNullStrings
