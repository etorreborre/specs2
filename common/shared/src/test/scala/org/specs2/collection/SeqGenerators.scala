package org.specs2.collection

import org.scalacheck.Gen

trait SeqGenerators {
  /**
   * @return a Generator for slices of a given sequence of elements.
   *
   * For example, `slicesOf(1, 2, 3, 4)` can produce `Seq(Seq(1, 2), Seq(3, 4))` or `Seq(Seq(1, 2, 3), Seq(4))`
   */
  def slicesOf[T](ts: T*): Gen[Seq[Seq[T]]] =
    slicesOfElements(ts)

  /**
   * @return a Generator for slices of a some elements out of a given sequence of elements.
   */
  def someSlicesOf[T](ts: T*): Gen[Seq[Seq[T]]] = for {
    tss <- Gen.someOf(ts)
    result <- slicesOfElements(tss.toList)
  } yield result

  private def slicesOfElements[T](ts: Seq[T]): Gen[Seq[Seq[T]]] = {
    def slice(indices: Seq[Int]): Seq[Seq[T]] = {
      indices.sliding(2).foldLeft(Seq.empty[Seq[T]]) { (res, cur) =>
        val (i, j) = (cur.head, cur.last)
        res :+ ts.slice(i, j)
      }
    }
    for {
      indices <- Gen.someOf(ts.indices)
    } yield slice(indices.toList.sorted)
  }


}

object SeqGenerators extends SeqGenerators