package org.specs2
package data

/**
 * A trait for anything that can be sized
 */
trait Sized[T]:
  /** @return the size of t */
  def size(t: T) : Int

  /** alias for size */
  def length(t: T) : Int = size(t)

object Sized extends SizedLowPriority1:
    /** any scala collection has a size */
  given [I <: Traversable[?]]: Sized[I] with
    def size(t: I) = t.size

trait SizedLowPriority1 extends SizedLowPriority2:
  /** any java collection has a size */
  given [T <: java.util.Collection[?]]: Sized[T] with
    def size(t: T) = t.size()

  /** any scala array has a size */
  given [T]: Sized[Array[T]] with
    def size(t: Array[T]) = t.length

trait SizedLowPriority2:

  /** a regular string has a size, without having to be converted to an Traversable */
  given Sized[String] with
    def size(t: String) = t.length
