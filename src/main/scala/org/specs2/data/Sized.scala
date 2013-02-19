package org.specs2
package data

/** a trait for anything that can be sized */
trait Sized[T] {
  def size(t: T) : Int
  /** alias for the size method */
  def length(t: T) : Int = size(t)
}
