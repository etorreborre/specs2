package org.specs2
package collection

import scala.collection.{GenSeq, GenIterable}
import Seqx._

/**
 * This trait provides additional methods on Iterable.
 *
 * It is made public so that users can reuse the sameElementsAs method
 */
trait Iterablex {
  /**
   * implicit definition to transform an Iterable to an ExtendedIterable
   */
  implicit def extendIterable[T](xs : GenIterable[T]): ExtendedIterable[T] = new ExtendedIterable(xs)

  /**
   * Additional methods for Iterable objects
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
     * This recursive function is not really well-formed (the `asInstanceOf` should be ample proof).
     * It only works if T <===> Seq[T]
     *
     * This is the case for NodeFunctions.isEqualIgnoringSpace where it is used to check if 2 xml NodeSeqs have the same
     * nodes regardless of whitespace
     *
     * @return true if the 2 iterables contain the same elements (according to a comparison function f) recursively, in any order
     */
    def sameElementsAs(that: GenIterable[T], f: (T, T) => Boolean): Boolean = {
      def isNotItsOwnIterable(a: GenIterable[_]) = a.isEmpty || a.iterator.next != a
      def matchTwo(x: T, y: T): Boolean = {
        (x, y) match {
          case (a: GenIterable[_], b: GenIterable[_]) if isNotItsOwnIterable(a) =>
            x.asInstanceOf[GenIterable[T]].sameElementsAs(y.asInstanceOf[GenIterable[T]], f)
          case _ => f(x, y)
        }
      }
      val ita = xs.iterator.toList
      val itb = that.iterator.toList
      (ita, itb) match {
        case (Nil, Nil) => true
        case (a: GenIterable[_], b: GenIterable[_]) =>
           (a.nonEmpty && b.nonEmpty) && {
            val (x, y, resta, restb) = (a.head, b.head, a.drop(1), b.drop(1))
            matchTwo(x, y) && resta.sameElementsAs(restb, f) ||
            resta.exists(matchTwo(_, y)) && restb.exists(matchTwo(x, _)) &&
              resta.removeFirst(matchTwo(_, y)).sameElementsAs(restb.removeFirst(matchTwo(x, _)), f)
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
      if (xs.nonEmpty && xs == xs.iterator.next)
        xs.toString
      else
        "[" + xs.toList.map {
          case i: GenIterable[_] => i.toDeepString
          case x => x.toString
        }.mkString(", ") + "]"
    }
    /** map the first element with a function */
    def mapFirst(f: T => T): GenSeq[T] = (xs.take(1).map(f) ++ xs.drop(1)).toSeq
    /** map the last element with a function */
    def mapLast(f: T => T): Seq[T] = (xs.seq.dropRight(1) ++ xs.seq.takeRight(1).map(f)).toSeq
    /** @return a sequence rotated of a number of elements */
    def rotate(n: Int) = xs.slice(n, xs.size) ++ xs.slice(0, n)
    /** @return a randomly mixed sequence */
    def scramble: Seq[T] = scramble(new scala.util.Random)

    /** @return a randomly mixed sequence */
    def scramble(random: scala.util.Random): Seq[T] =
      // rotate arbitrarily the sequence first then sort randomly
      xs.rotate(random.nextInt(xs.size+1)).seq.toSeq.sortWith((_,_) => random.nextInt(2) > 0)

  }
}

object Iterablex extends Iterablex
