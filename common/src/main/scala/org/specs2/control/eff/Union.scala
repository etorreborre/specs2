package org.specs2.control.eff

import Effects._

import scalaz.Scalaz._
import scalaz._

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
trait Union[R, A] {
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
      case UnionNow(tv)     => tv.right
      case UnionNext(union) => union.left
    }

}

/**
 * Member typeclass for effects belonging to a stack of effects R
 *
 * If T is a member of R then we can:
 *
 * - create a Union of effects from a single effect with "inject"
 * - extract an effect value from a union if there is such an effect in the stack
 */
trait Member[T[_], R] {
  def inject[V](tv: T[V]): Union[R, V]

  def project[V](u: Union[R, V]): Option[T[V]]
}

object Member {

  /**
   * Implicits for determining if an effect T is member of a stack R
   *
   * Member uses MemberNat which tracks the "depth" of the effect in the stack R
   * using type-level naturals
   */
  implicit def MemberNatIsMember[T[_], R <: Effects, N <: Nat](implicit m: MemberNat[T, R, N], p: P[N]): Member[T, R] = new Member[T, R] {
    def inject[V](tv: T[V]): Union[R, V] =
      m.inject(p, tv)

    def project[V](u: Union[R, V]): Option[T[V]] =
      m.project(p, u)
  }

  /**
   * helper method to untag a tagged effect
   */
  def untagMember[T[_], R, TT](m: Member[({type X[A]=T[A] @@ TT})#X, R]): Member[T, R] =
    new Member[T, R] {
      def inject[V](tv: T[V]): Union[R, V] =
        m.inject(Tag(tv))

      def project[V](u: Union[R, V]): Option[T[V]] =
        m.project(u).map(Tag.unwrap)
    }

  /**
   * Syntactic sugar for the Member type
   *
   * implicit m: Member[M, R]
   * implicit m: M <= R
   *
   */
  type <=[M[_], R] = Member[M, R]

}


/**
 * The rank of a member effects is modelled as a type-level natural
 * and modelled by a value of that type
 */
trait MemberNat[T[_], R <: Effects, N <: Nat] {
  def inject[V](rank: P[N], effect: T[V]): Union[R, V]

  def project[V](rank: P[N], union: Union[R, V]): Option[T[V]]
}

object MemberNat {

  implicit def ZeroMemberNat[T[_], R <: Effects]: MemberNat[T, T |: R, Zero] = new MemberNat[T, T |: R, Zero] {
    def inject[V](rank: P[Zero], effect: T[V]): Union[T |: R, V] =
      Union.now(effect)

    def project[V](predicate: P[Zero], union: Union[T |: R, V]): Option[T[V]] =
      union match {
        case UnionNow(x) => Some(x)
        case _ => None
      }
  }

  implicit def SuccessorMemberNat[T[_], O[_], R <: Effects, N <: Nat](implicit m: MemberNat[T, R, N]): MemberNat[T, O |: R, Succ[N]] = new MemberNat[T, O |: R, Succ[N]] {
    def inject[V](predicate: P[Succ[N]], effect: T[V]) =
      Union.next(m.inject[V](P[N](), effect))

    def project[V](predicate: P[Succ[N]], union: Union[O |: R, V]) =
      union match {
        case UnionNow(_) => None
        case UnionNext(u) => m.project[V](P[N](), u)
      }
  }

}

/**
 * type level naturals
 */
sealed trait Nat

trait Zero extends Nat
trait Succ[N <: Nat] extends Nat

/**
 * values with a phantom type representing a type-level natural
 */
case class P[N <: Nat]()

object P {

  implicit def ZeroPredicate: P[Zero] =
    P[Zero]

  implicit def SuccPredicate[N <: Nat](implicit prev: P[N]): P[Succ[N]] =
    P[Succ[N]]
}

