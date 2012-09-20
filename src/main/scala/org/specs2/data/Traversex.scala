package org.specs2.data

import org.specs2.internal.scalaz._
import Scalaz._
/**
 * This trait adds Traverse instances and convenience methods on collections
 */
private[specs2]
trait Traversex {

  implicit def toReducableMonoid[T](seq: Seq[T]): ReducableMonoid[T] = new ReducableMonoid[T](seq)
  class ReducableMonoid[T](seq: Seq[T]) {
    def reduceMonoid[M : Monoid](reducer: T => M) = {
      seq//.toStream.traverseU(reducer)
    }
  }
}

private[specs2]
object Traversex extends Traversex
