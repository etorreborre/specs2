package org.specs2
package data

import internal.scalaz._
import Scalaz._

private [specs2]
trait Reducerx {
  implicit def extendReducer[T, M : Monoid](r: Reducer[T, M]) = new ExtendedReducer[T, M](r)

  case class ExtendedReducer[T, M : Monoid](r: Reducer[T, M]) {
    /**
     * compose a reducer with another one
     */
    def >>>(to: Reducer[M, M]) = new Reducer[T, M] {
      override def snoc(m: M, t: T): M = {
        val result = r.snoc(m, t)
        val effect = to.unit(result)
        result
      }
    }
  }
}

object Reducerx extends Reducerx {
  implicit def semigroupIsOptionMonoid[T : Semigroup]: Monoid[Option[T]] = new Monoid[Option[T]] {
    def append(t1: Option[T], t2: =>Option[T]) = (t1 <**> t2)(implicitly[Semigroup[T]].append(_,_))
    val zero: Option[T] = None
  }
}