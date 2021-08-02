package org.specs2
package collection

import scala.collection.{GenSeq, GenIterable}
import Seqx.*

given canEqualAny[L, R]: CanEqual[L, R] = CanEqual.derived

/**
 * This trait provides additional methods on Iterable.
 *
 * It is made public so that users can reuse the sameElementsAs method
 */
trait Iterablex:
  /**
   * Extension methods for Iterables
   */
  extension [T, S >: T](xs : GenIterable[T])

    /**
     * @return true if the 2 iterables contain the same elements, in the same order,
     *         according to a function f
     */
    def isSimilar(that: GenIterable[S], f: Function2[T, S, Boolean]): Boolean =
      val it1 = xs.iterator
      val it2 = that.iterator
      var res = true
      while res && it1.hasNext && it2.hasNext do
        res = f(it1.next, it2.next)
      !it1.hasNext && !it2.hasNext && res


  extension [T](xs : GenIterable[T])

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
    def sameElementsAs(that: GenIterable[T], f: (T, T) => Boolean): Boolean =
      def isNotItsOwnIterable(a: GenIterable[Any]): Boolean =
        a.isEmpty || (a.iterator.next:Any) != (a:Any)

      def matchTwo(x: T, y: T): Boolean =
        (x.asInstanceOf[Matchable], y.asInstanceOf[Matchable]) match
          case (a: GenIterable[?], b: GenIterable[?]) if isNotItsOwnIterable(a) =>
            x.asInstanceOf[GenIterable[T]].sameElementsAs(y.asInstanceOf[GenIterable[T]], f)
          case _ => f(x, y)

      val ita = xs.iterator.toList
      val itb = that.iterator.toList

      (ita, itb) match
        case (List(), List()) => true
        case (a: GenIterable[?], b: GenIterable[?]) =>
           (a.nonEmpty && b.nonEmpty) && {
            val (x, y, resta, restb) = (a.head, b.head, a.drop(1), b.drop(1))
            matchTwo(x, y) && resta.sameElementsAs(restb, f) ||
            resta.exists(matchTwo(_, y)) && restb.exists(matchTwo(x, _)) &&
              resta.toSeq.removeFirst(matchTwo(_, y)).sameElementsAs(restb.toSeq.removeFirst(matchTwo(x, _)), f)
          }
          
    /**
     * @return true if the second iterable elements are contained in the first, in order
     */
    def containsInOrder(l: T*): Boolean =
      val firstList = xs.toList
      val secondList = l.toList
      (firstList, secondList) match
         case (_, Nil) => true
         case (Nil, _) => false
         case (a :: Nil, b :: Nil) => a == b
         case (a :: firstRest, b :: secondRest) => {
           if a != b then
             firstRest.containsInOrder(secondList*)
           else
             firstRest.containsInOrder(secondRest*)
         }

    /**
     * @return the representation of the elements of the iterable using the toString method recursively
     */
    def toDeepString: String =
      if xs.nonEmpty && (xs:Any) == (xs.iterator.next:Any) then
        xs.toString
      else
        "[" + xs.toList.map { i =>
          i.asInstanceOf[Matchable] match {
            case x: GenIterable[?] => x.toDeepString
            case x => x.toString
          }
        }.mkString(", ") + "]"

    /** map the first element with a function */
    def mapFirst(f: T => T): GenSeq[T] = (xs.take(1).map(f) ++ xs.drop(1)).toSeq

    /** map the last element with a function */
    def mapLast(f: T => T): Seq[T] = (xs.dropRight(1) ++ xs.takeRight(1).map(f)).toSeq

    /** @return a sequence rotated of a number of elements */
    def rotate(n: Int) = xs.slice(n, xs.size) ++ xs.slice(0, n)

    /** @return a randomly mixed sequence */
    def scramble: Seq[T] = scramble(new scala.util.Random)

    /** @return a randomly mixed sequence */
    def scramble(random: scala.util.Random): Seq[T] =
      // rotate arbitrarily the sequence first then sort randomly
      xs.rotate(random.nextInt(xs.size+1)).toSeq.sortWith((_,_) => random.nextInt(2) > 0)


object Iterablex extends Iterablex
