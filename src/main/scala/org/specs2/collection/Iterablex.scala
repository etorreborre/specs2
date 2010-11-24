package org.specs2
package collection
import Listx._

/**
 * This trait provides additional methods on Iterable
 */
private[specs2]
trait Iterablex {
  /**
   * implicit definition to transform an iterable to an ExtendedIterable
   */
  implicit def extendIterable[T](xs : Iterable[T]): ExtendedIterable[T] = new ExtendedIterable(xs)

  /**
   * See the description of the ExtendedIterable object
   */
  class ExtendedIterable[T](xs:Iterable[T]) {

    /**
     * @return true if the 2 iterables contain the same elements, in the same order, 
     *         according to a function f
     */
    def isSimilar[S >: T](that: Iterable[S], f: Function2[T, S, Boolean]): Boolean = {
      val it1 = xs.iterator
      val it2 = that.iterator
      var res = true
      while (res && it1.hasNext && it2.hasNext) {
        res = f(it1.next, it2.next)
      }
      !it1.hasNext && !it2.hasNext && res
    }
    /**
     * @return true if the 2 iterables contain the same elements recursively, in any order
     */
    def sameElementsAs(that: Iterable[T]): Boolean = sameElementsAs(that, (x, y) => x == y)
    /**
     * @return true if the 2 iterables contain the same elements (according to a comparison function f) recursively, in any order
     */
    def sameElementsAs(that: Iterable[T], f: (T, T) => Boolean): Boolean = {
      def isNotItsOwnIterable(a: Iterable[_]) = a.isEmpty || a.iterator.next != a
      def matchTwo(x: T, y: T): Boolean = {
        (x, y) match {
          case (a: Iterable[_], b:Iterable[_]) if (isNotItsOwnIterable(a)) => x.asInstanceOf[Iterable[T]].sameElementsAs(y.asInstanceOf[Iterable[T]], f)
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
    /**
     * @return true if the second iterable elements are contained in the first, in order
     */
    def containsInOrder(l: T*): Boolean = {
      val firstList = xs.toList
      val secondList = l.toList
      (firstList, secondList) match {
         case (_, Nil) => true
         case (Nil, _) => false
         case (a :: Nil, b :: Nil) => a == b
         case (a :: firstRest, b :: secondRest) => {
           if (a != b)
             firstRest.containsInOrder(secondList:_*)
           else
             firstRest.containsInOrder(secondRest:_*)
         }
      }
    }
  }
}
private[specs2]
object Iterablex extends Iterablex