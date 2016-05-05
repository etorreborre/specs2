package org.specs2.control.eff

import scalaz._
import Effects._

/**
 * Open union of effects
 *
 * They are modelled as a list of
 *
 *  UnionNext(UnionNext(...(UnionNow(M[X])))
 *
 * where M[X] is an effect. The depth of the nesting in an Union value
 * corresponds to the place of the effect in a type E1 |: E2 |: E3 |: .. |: NoEffect
 *
 */
sealed trait Union[+R, A] {
  type X = A
}

case class UnionNow[T[_], R <: Effects, A](ta: T[A]) extends Union[T |: R, A]

case class UnionNext[O[_], R <: Effects, A](u: Union[R, A]) extends Union[O |: R, A]

/**
 * create union objects
 */
object Union {
  def now[T[_], R <: Effects, A](ta: T[A]): Union[T |: R, A] =
    UnionNow(ta)

  def next[O[_], R <: Effects, A](u: Union[R, A]): Union[O |: R, A] =
    UnionNext(u)

  /**
   * decompose a union starting with a given effect into
   *
   *  - a value for that effect type if there is one
   *  - the union with the remaining effects
   */
  def decompose[T[_], R <: Effects, V](u: Union[T |: R, V]): Union[R, V] \/ T[V] =
    u match {
      case UnionNow(tv)     => \/-(tv)
      case UnionNext(union) => -\/(union)
    }

}
