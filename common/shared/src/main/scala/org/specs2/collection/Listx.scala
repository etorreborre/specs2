package org.specs2
package collection

import scala.annotation.tailrec
import scala.collection.immutable.List._

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
    def safeTranspose = outer.transpose(list)
  }
  
  /** @return an extension for a list */
  implicit def extendList[T](list: List[T]): ExtendedList[T] = new ExtendedList(list)
  /**
   * Additional methods for lists
   */
  class ExtendedList[T](list: List[T]) {
    /**
     * @return a randomly mixed list
     */
    def scramble = list.sortWith((a, b) => (new java.util.Random).nextInt(1) > 0)

    def intersperse[A](a: T): List[T] = {
      @tailrec
      def intersperse0(accum: List[T], rest: List[T]): List[T] = rest match {
        case Nil      => accum
        case x :: Nil => x :: accum
        case h :: t   => intersperse0(a :: h :: accum, t)
      }
      intersperse0(Nil, list).reverse
    }

  }
  
  /**
   * This methods works like the transpose method defined on Traversable
   * but it doesn't fail when the input is not formatted like a regular matrix
   * 
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
