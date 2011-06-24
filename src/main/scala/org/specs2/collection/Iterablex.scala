package org.specs2
package collection
import Listx._
import scala.collection.{GenSeq, GenIterable}

/**
* This trait provides additional methods on Iterable
*/
private[specs2]
trait Iterablex {
  /**
   * implicit definition to transform an iterable to an ExtendedIterable
   */
  implicit def extendIterable[T](xs : GenIterable[T]): ExtendedIterable[T] = new ExtendedIterable(xs)

  /**
   * See the description of the ExtendedIterable object
   */
  class ExtendedIterable[T](xs: GenIterable[T]) {

    /**
     * @return true if the 2 iterables contain the same elements, in the same order, 
     *         according to a function f
     */
    def isSimilar[S >: T](that: GenIterable[S], f: Function2[T, S, Boolean]): Boolean = {
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
    def sameElementsAs(that: GenIterable[T]): Boolean = sameElementsAs(that, (x, y) => x == y)
    /**
     * @return true if the 2 iterables contain the same elements (according to a comparison function f) recursively, in any order
     */
    def sameElementsAs(that: GenIterable[T], f: (T, T) => Boolean): Boolean = {
      def isNotItsOwnIterable(a: GenIterable[_]) = a.isEmpty || a.iterator.next != a
      def matchTwo(x: T, y: T): Boolean = {
        (x, y) match {
          case (a: GenIterable[_], b: GenIterable[_]) if (isNotItsOwnIterable(a)) => x.asInstanceOf[GenIterable[T]].sameElementsAs(y.asInstanceOf[GenIterable[T]], f)
          case _ => f(x, y)
        }
      }
      val ita = xs.iterator.toList
      val itb = that.iterator.toList
      var res = true
      (ita, itb) match {
        case (Nil, Nil) => true
        case (a: GenIterable[_], b: GenIterable[_]) => {
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
    /**
     * @return the representation of the elements of the iterable using the toString method recursively
     */
    def toDeepString: String = {
      if (!xs.isEmpty && xs == xs.iterator.next)
        xs.toString
      else
        "[" + xs.toList.map { x => 
          x match {
            case i: GenIterable[_] => i.toDeepString
            case _ => x.toString
          }
       }.mkString(", ") + "]"
    }
    /** map the first element with a function */
    def mapFirst(f: T => T): GenSeq[T] = (xs.take(1).map(f) ++ xs.drop(1)).toSeq
    /** map the last element with a function */
    def mapLast(f: T => T): Seq[T] = (xs.seq.dropRight(1) ++ xs.seq.takeRight(1).map(f)).toSeq
  }
}
private[specs2]
object Iterablex extends Iterablex