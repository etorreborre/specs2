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
        val effect = to.unit(r.snoc(m, t))
        m |+| effect
      }
    }
  }

  implicit lazy val unitIsMonoid = new Monoid[Unit] {
    val zero = ()
    def append(u: Unit, u2: =>Unit) = u2
  }
}

object Reducerx extends Reducerx