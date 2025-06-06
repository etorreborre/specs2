package org.specs2
package collection

import org.specs2.fp.*
import scala.collection.mutable.ListBuffer

/** This trait provides additional methods on Seqs and nested Seqs
  */
private[specs2] trait Seqx:

  /** Additional methods for nested seqs
    */
  extension [T](seq: Seq[Seq[T]])
    def safeTranspose: Seq[Seq[T]] =
      transpose(seq)

  /** Additional methods for seqs
    */
  extension [T](seq: Seq[T])

    /** update the last element if there is one */
    def updateLast(f: T => T) = seq match
      case s :+ last => s :+ f(last)
      case other     => other

    /** update the last element or start the sequence with a new init value */
    def updateLastOr(f: PartialFunction[T, T])(initValue: =>T) = seq match
      case s :+ last => s :+ f(last)
      case other     => seq :+ initValue

    /** remove the first element satisfying the predicate
      * @return
      *   a seq minus the first element satisfying the predicate
      */
    def removeFirst(predicate: T => Boolean): Seq[T] =
      val (withoutElement, startWithElement) = seq span (x => !predicate(x))
      withoutElement ++ startWithElement.drop(1)

    /** This implementation reuses the Seq.diff implementation but with a user-defined equality
      * @return
      *   remove all the elements of other from seq with a user-defined equality function
      */
    def difference(other: Seq[T], equality: (T, T) => Boolean = (_: T) == (_: T)): scala.collection.Seq[T] =

      def occurrenceCounts(sq: Seq[T], equality: (T, T) => Boolean): scala.collection.mutable.Map[D[T], Int] =
        val occurrences = new scala.collection.mutable.HashMap[D[T], Int] { override def default(k: D[T]) = 0 }
        for y <- sq do occurrences(D(y, equality)) += 1
        occurrences

      val occurrences = occurrenceCounts(other, equality)
      val result = new ListBuffer[T]
      for x <- seq do
        if occurrences(D(x, equality)) == 0 then result += x
        else occurrences(D(x, equality)) -= 1
      result.toSeq

  extension [T, S](seq: Seq[T])
    /** @return
      *   all the elements in seq which are not in other, even if they are duplicates: Seq(1, 1).delta(Seq(1)) == Seq(1)
      *   this uses a user given comparison function
      */
    def delta(other: Seq[S], compare: (T, S) => Boolean): Seq[T] =
      def notFound(ls1: Seq[T], ls2: Seq[S], result: Seq[T] = Seq()): Seq[T] =
        ls1 match
          case Seq()        => result
          case head +: rest =>
            if ls2.exists(compare(head, _)) then notFound(rest, ls2.removeFirst(l => compare(head, l)), result)
            else notFound(rest, ls2, result :+ head)
      notFound(seq, other)

  /** This methods works like the transpose method defined on Traversable but it doesn't fail when the input is not
    * formatted like a regular matrix
    *
    * List(List("a", "bb", "ccc"), List("dd", "e", "fff")) => List(List("a", "dd"), List("e", "bb") List("ccc", "fff"))
    */
  def transpose[T](xs: Seq[Seq[T]]): Seq[Seq[T]] =
    val filtered = xs.filter(_.nonEmpty)
    if filtered.isEmpty then Seq()
    else filtered.map(_.head) +: transpose(filtered.map(_.tail))

  given Foldable[Seq] with
    def foldLeft[A, B](fa: Seq[A], z: B)(f: (B, A) => B) =
      summon[Foldable[List]].foldLeft(fa.toList, z)(f)

    def foldRight[A, B](fa: Seq[A], z: =>B)(f: (A, =>B) => B) =
      summon[Foldable[List]].foldRight(fa.toList, z)(f)

    def foldMap[A, B](fa: Seq[A])(f: (A) => B)(using F: Monoid[B]) =
      summon[Foldable[List]].foldMap(fa.toList)(f)

private[specs2] object Seqx extends Seqx

// Helper class for counting occurrences
private case class D[T](t: T, equality: (T, T) => Boolean):
  override def equals(o: Any) = o.asInstanceOf[Matchable] match
    // it's ok to remove the warning here since we are always going compare values of the same type
    case other: D[T] @unchecked => equality(t, other.t)
    case _                      => false
  // always return the same hashcode because we can't guarantee that if equality(t1, t2) == true then t1.hashCode == t2.hashCode
  // this however makes the search much less efficient
  override def hashCode = 1
