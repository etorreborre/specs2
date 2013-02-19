package org.specs2
package data

import internal.scalaz._
import Scalaz._

private [specs2]
trait Reducerx {
  implicit def extendReducer[T, M : Monoid](r: Reducer[T, M]) = new ExtendedReducer[T, M](r)

  case class ExtendedReducer[T, M : Monoid](r: Reducer[T, M]) {
    /**
     * add side-effects to a reducer
     */
    def >>>(doEffect: M => M): Reducer[T, M] =
      Reducer.reducer (
        u = r.unit(_),
        cs = (t: T) => (m: M) => r.cons(t, m),
        sc = (m: M) => (t: T) => doEffect(r.snoc(m, t))
      )
    /**
     * pair the results of a reducer with another one
     */
    def &&&[M2 : Monoid](other: Reducer[T, M2]): Reducer[T, (M, M2)] = r compose other
  }
}

object Reducerx extends Reducerx {
  implicit def semigroupIsOptionMonoid[T : Semigroup]: Monoid[Option[T]] = new Monoid[Option[T]] {
    def append(t1: Option[T], t2: =>Option[T]) = ^(t1, t2)(implicitly[Semigroup[T]].append(_,_))
    val zero: Option[T] = None
  }
}