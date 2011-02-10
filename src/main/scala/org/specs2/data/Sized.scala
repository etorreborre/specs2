package org.specs2
package data

/** a trait for anything that can be sized */
trait Sized[T] {
  def size(t: T) : Int
}
