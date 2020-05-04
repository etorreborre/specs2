package org.specs2.collection

import scala.annotation.tailrec

/**
 * This trait provides additional methods on Vectors and nested Vectors
 */
private[specs2]
trait Vectorx { outer =>

  /**
   * Additional methods for nested vectors
   */
  implicit class ExtendedNestedVector[T](vector: Vector[Vector[T]]) {
    def safeTranspose = outer.transpose(vector)
  }

  /**
   * Additional methods for vectors
   */
  implicit class ExtendedVector[T](vector: Vector[T]) {
    /**
     * @return a randomly mixed vector
     */
    def scramble = vector.sortWith((a, b) => (new java.util.Random).nextInt(1) > 0)

    def intersperse[A](a: T): Vector[T] = {
      @tailrec
      def intersperse0(accum: Vector[T], rest: Vector[T]): Vector[T] = rest match {
        case Vector()  => accum
        case Vector(x) => x +: accum
        case v    => intersperse0(a +: v.head +: accum, v.tail)
      }
      intersperse0(Vector(), vector).reverse
    }

  }

  /**
   * This methods works like the transpose method defined on Traversable
   * but it doesn't fail when the input is not formatted like a regular matrix
   *
   *  Vector(Vector("a",  "bb", "ccc"),
   *       Vector("dd", "e",  "fff")) =>
   *  Vector(Vector("a",  "dd"),
   *       Vector("e",  "bb")
   *       Vector("ccc",  "fff"))
   */
  def transpose[T](xs: Vector[Vector[T]]): Vector[Vector[T]] = {
    val filtered = xs.filter(_.nonEmpty)
    if (filtered.isEmpty) Vector()
    else filtered.map(_.head) +: transpose(filtered.map(_.tail))
  }
}

private[specs2]
object Vectorx extends Vectorx
