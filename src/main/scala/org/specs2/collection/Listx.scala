package org.specs2
package collection

/**
 * This trait provides additional methods on Lists and nested Lists
 */
private[specs2]
trait Listx { outer =>
  /** @return an extension for a nested list */
  implicit def extendNestedList[T](list: List[List[T]]): ExtendedNestedList[T] = new ExtendedNestedList(list)
  /**
   * Additional methods for nested lists
   */
  class ExtendedNestedList[T](list: List[List[T]]) {
    def transpose = outer.transpose(list)
  }
  
  /** @return an extension for a list */
  implicit def extendList[T](list: List[T]): ExtendedList[T] = new ExtendedList(list)
  /**
   * Additional methods for lists
   */
  class ExtendedList[T](list: List[T]) {
    /**
     * remove the first element satisfying the predicate
     * @return a list minus the first element satisfying the predicate
     */
    def removeFirst(predicate: T => Boolean): List[T] = {
      list match {
        case Nil => Nil
        case x :: rest if (predicate(x)) => rest
        case x :: rest if (!predicate(x)) => List(x) ::: rest.removeFirst(predicate)
        case _ => Nil // should never happen thanks to the predicate condition above
      }
    }
    /**
     * @return a randomly mixed list
     */
    def scramble = list.sortWith((a, b) => (new java.util.Random).nextInt(1) > 0)
  }
  
  /**
   *  List(List("a",  "bb", "ccc"),
   *       List("dd", "e",  "fff")) =>
   *  List(List("a",  "dd"),
   *       List("e",  "bb")
   *       List("ccc",  "fff"))
   */
  def transpose[T](xs: List[List[T]]): List[List[T]] = {
    val filtered = xs.filter(_.nonEmpty)
    if (filtered.isEmpty) Nil
    else filtered.map(_.head) :: transpose(filtered.map(_.tail))
  }
}
private[specs2]
object Listx extends Listx