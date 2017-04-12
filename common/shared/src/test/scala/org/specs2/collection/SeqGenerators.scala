package org.specs2.collection

import org.scalacheck.Gen

trait SeqGenerators {
  /**
   * @return a Generator for slices of a given sequence of elements.
   *
   * For example, `slicesOf(1, 2, 3, 4)` can produce `Seq(Seq(1, 2), Seq(3, 4))` or `Seq(Seq(1, 2, 3), Seq(4))`
   */
  def slicesOf[T](ts: T*): Gen[Seq[Seq[T]]] = {
    def slice(indices: Seq[Int]) = {
      indices.sliding(2).foldLeft(Seq[Seq[T]]()) { (res, cur) =>
        val (i, j) = (cur.head, cur.last)
        res ++ Seq(ts.slice(i, j))
      }
    }
    for {
      indices <- Gen.someOf((0 until ts.size).toSeq)
    } yield slice(indices.sorted)
  }

  /**
   * @return a Generator for slices of a some elements out of a given sequence of elements.
   */
  def someSlicesOf[T](ts: T*): Gen[Seq[Seq[T]]] = for {
    tss <- Gen.someOf(ts)
    result <- slicesOf(tss:_*)
  } yield result
}

object SeqGenerators extends SeqGenerators