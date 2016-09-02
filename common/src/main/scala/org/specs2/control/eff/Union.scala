package org.specs2.control.eff

/**
 * Union represents one effect T[_] embedded in a tree of possible effects R
 *
 * Since the effect tree is represented with the following cases:
 *   - Fx1[T]
 *   - Fx2[T1, T2]
 *   - Fx3[T1, T2, T3]
 *   - FxAppend[L, R]
 *
 * We have the corresponding Union cases. For example
 *   T2 is in the "middle" of Fx3[T1, T2, T3] so creating a Union object for that effect uses Union3M
 */
sealed trait Union[+R, A] {
  type X = A
}

case class Union1[T[_], A](ta: T[A]) extends Union[Fx1[T], A]

sealed trait Union2[R, A] extends Union[R, A]
case class Union2L[L[_], R[_], A](t: L[A]) extends Union2[Fx2[L, R], A]
case class Union2R[L[_], R[_], A](t: R[A]) extends Union2[Fx2[L, R], A]

sealed trait Union3[R, A] extends Union[R, A]
case class Union3L[L[_], M[_], R[_], A](t: L[A]) extends Union3[Fx3[L, M, R], A]
case class Union3M[L[_], M[_], R[_], A](t: M[A]) extends Union3[Fx3[L, M, R], A]
case class Union3R[L[_], M[_], R[_], A](t: R[A]) extends Union3[Fx3[L, M, R], A]

sealed trait UnionAppend[R, A] extends Union[R, A]
case class UnionAppendL[L, R, A](t: Union[L, A]) extends UnionAppend[FxAppend[L, R], A]
case class UnionAppendR[L, R, A](t: Union[R, A]) extends UnionAppend[FxAppend[L, R], A]

