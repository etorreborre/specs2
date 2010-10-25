package org.specs2
package collection
import Listx._

trait Iterablex {
  /**
   * implicit definition to transform an iterable to an ExtendedIterable
   */
  implicit def extend[A](xs : Iterable[A]) = new ExtendedIterable(xs)

  /**
   * See the description of the ExtendedIterable object
   */
  class ExtendedIterable[A](xs:Iterable[A]) {

    /**
     * @return true if the 2 iterables contain the same elements, in the same order, according to a function f
     */
    def isSimilar[B >: A](that: Iterable[B], f: Function2[A, B, Boolean]): Boolean = {
      val ita = xs.iterator
      val itb = that.iterator
      var res = true
      while (res && ita.hasNext && itb.hasNext) {
        res = f(ita.next, itb.next)
      }
      !ita.hasNext && !itb.hasNext && res
    }
    /**
     * @return true if the second iterable elements are contained in the first, in order
     */
    def containsInOrder[A](l: A*): Boolean = {
      val firstList = xs.toList
      val secondList = l.toList
      (firstList, secondList) match {
         case (_, Nil) => true
         case (Nil, _) => false
         case (a :: Nil, b :: Nil) => a == b
         case (a :: firstRest, b :: secondRest) => {
           if (a != b)
             firstRest.containsInOrder(secondList)
           else
             firstRest.containsInOrder(secondRest)
         }
      }
    }
    /**
     * @return true if the 2 iterables contain the same elements recursively, in any order
     */
    def sameElementsAs(that: Iterable[A]): Boolean = sameElementsAs(that, (x, y) => x == y)
    /**
     * @return true if the 2 iterables contain the same elements (according to a comparison function f) recursively, in any order
     */
    def sameElementsAs(that: Iterable[A], f: (A, A) => Boolean): Boolean = {
      def isNotItsOwnIterable(a: Iterable[_]) = a.isEmpty || a.iterator.next != a
      def matchTwo(x: A, y: A): Boolean = {
        (x, y) match {
          case (a: Iterable[_], b:Iterable[_]) if (isNotItsOwnIterable(a)) => x.asInstanceOf[Iterable[A]].sameElementsAs(y.asInstanceOf[Iterable[A]], f)
          case _ => f(x, y)
        }
      }
      val ita = xs.iterator.toList
      val itb = that.iterator.toList
      var res = true
      (ita, itb) match {
        case (Nil, Nil) => true
        case (a: Iterable[_], b: Iterable[_]) => {
          if (a.headOption.isDefined && b.headOption.isDefined) {
            val (x, y, resta, restb) = (a.head, b.head, a.drop(1), b.drop(1))
            matchTwo(x, y) && resta.sameElementsAs(restb, f) ||
            resta.exists(matchTwo(_, y)) && restb.exists(matchTwo(_, x)) &&
              resta.removeFirst(matchTwo(_, y)).sameElementsAs(restb.removeFirst(matchTwo(_, x)), f)
          }
          else
            false
        }
        case _ => ita == itb
      }
    }
  }
}
object Iterablex extends Iterablex