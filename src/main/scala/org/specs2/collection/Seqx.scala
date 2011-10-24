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
private[specs2]
object Seqx extends Seqx