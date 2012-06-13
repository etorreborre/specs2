package org.specs2.data

import org.specs2.internal.scalaz._
import Scalaz._
import Applicative._
import Traverse._

/**
 * This trait adds Traverse instances and convenience methods on collections
 */
private[specs2]
trait Traversex {
  implicit def toReducableMonoid[T](seq: Seq[T]): ReducableMonoid[T] = new ReducableMonoid[T](seq)
  class ReducableMonoid[T](seq: Seq[T]) {
    def reduceMonoid[M : Monoid](reducer: T => M) = {
      val f = (t: T) => Const[M, T](reducer(t))
      seq.traverse[({type l[T]=Const[M, T]})#l, T](f).value
    }
  }
}

private[specs2]
object Traversex extends Traversex
