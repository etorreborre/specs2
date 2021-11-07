package org.specs2
package data

/**
 * A trait for anything that can be sized
 */
trait Sized[T] {
  /** @return the size of t */
  def size(t: T) : Int

  /** alias for size */
  def length(t: T) : Int = size(t)

  def isEmpty(t: T): Boolean =
    size(t) == 0
}

object Sized {

  def apply[T](implicit sized: Sized[T]): Sized[T] =
    sized

  /** any scala collection has a size */
  implicit def scalaTraversableIsSized[I <: Traversable[?]]: Sized[I] = new Sized[I] {
    def size(t: I) = t.size
  }
  /** any scala array has a size */
  implicit def scalaArrayIsSized[T]: Sized[Array[T]] = new Sized[Array[T]] {
    def size(t: Array[T]) = t.length
  }
  /** any java collection has a size */
  implicit def javaCollectionIsSized[T <: java.util.Collection[?]]: Sized[T] = new Sized[T] {
    def size(t: T) = t.size()
  }
  /** a regular string has a size, without having to be converted to an Traversable */
  implicit def stringIsSized: Sized[String] = new Sized[String] {
    def size(t: String) = t.length
  }

  implicit class SizedOps[T](t: T)(implicit sized: Sized[T]) {
    def size: Int =
      sized.size(t)

    def length: Int =
      sized.length(t)

    def isEmpty: Boolean =
      sized.isEmpty(t)
  }

}
