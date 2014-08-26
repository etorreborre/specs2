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
}
