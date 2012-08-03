package org.specs2
package collection

import internal.scalaz._
import Generator._

/**
 * This trait provides additional methods on Seqs and nested Seqs
 */
private[specs2]
trait Seqx { outer =>

  /** @return an extension for a nested seq */
  implicit def extendNestedSeq[T](seq: Seq[Seq[T]]): ExtendedNestedSeq[T] = new ExtendedNestedSeq(seq)
  /**
   * Additional methods for nested seqs
   */
  class ExtendedNestedSeq[T](seq: Seq[Seq[T]]) {
    def safeTranspose = outer.transpose(seq)
  }
  /** @return an extension for a seq */
  implicit def extendSeq[T](seq: Seq[T]): ExtendedSeq[T] = new ExtendedSeq(seq)
  /**
   * Additional methods for seqs
   */
  class ExtendedSeq[T](seq: Seq[T]) {

    def reduceWith[S](reducer: Reducer[T, S]) = FoldlGenerator[Seq].reduce(reducer, seq)

    /** update the last element if there is one */
    def updateLast(f: T => T) = seq match {
      case s :+ last => s :+ f(last)
      case other     => other
    }

    /** update the last element or start the sequence with a new init value */
    def updateLastOr(f: PartialFunction[T, T])(initValue: =>T) = seq match {
      case s :+ last => s :+ f(last)
      case other     => seq :+ initValue
    }

    /**
     * remove the first element satisfying the predicate
     * @return a seq minus the first element satisfying the predicate
     */
    def removeFirst(predicate: T => Boolean): Seq[T] = {
      val (withoutElement, startWithElement) = seq span (x => !predicate(x))
      withoutElement ++ startWithElement.drop(1)
    }

    /**
     * @return all the elements in seq which are not in other, even if they are duplicates: Seq(1, 1).diff(Seq(1)) == Seq(1)
     *         this uses a user given comparison function
     */
    def delta[S](other: Seq[S], compare: (T, S) => Boolean): Seq[T] = {
      def notFound(ls1: Seq[T], ls2: Seq[S], result: Seq[T] = Seq()): Seq[T] =
        ls1 match {
          case Seq()        => result
          case head +: rest =>
            if  (ls2.exists(compare(head, _))) notFound(rest, ls2.removeFirst(l => compare(head, l)), result)
            else                               notFound(rest, ls2, result :+ head)
        }
      notFound(seq, other)
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
  def transpose[T](xs: Seq[Seq[T]]): Seq[Seq[T]] = {
    val filtered = xs.filter(_.nonEmpty)
    if (filtered.isEmpty) Seq()
    else filtered.map(_.head) +: transpose(filtered.map(_.tail))
  }
}

/**
 * extrator for the first element of Seq[T]
 */
object +: {
  def unapply[T](l: Seq[T]): Option[(T, Seq[T])] = {
    if(l.isEmpty) None
    else          Some(l.head, l.tail)
  }
}

/**
 * extrator for the last element of Seq[T]
 */
object :+ {
  def unapply[T](l: Seq[T]): Option[(Seq[T], T)] = {
    if(l.isEmpty) None
    else          Some(l.init, l.last)
  }
}

private[specs2]
object Seqx extends Seqx