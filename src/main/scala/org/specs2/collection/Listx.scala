package org.specs2
package collection

trait Listx { outer =>
  implicit def extend[T](list: List[List[T]]): ExtendedNestedList[T] = new ExtendedNestedList(list)
  class ExtendedNestedList[T](list: List[List[T]]) {
    def transpose = outer.transpose(list)
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
object Listx extends Listx