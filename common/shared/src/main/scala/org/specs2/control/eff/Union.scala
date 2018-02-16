package org.specs2
package control.eff

/**
 * Union represents one effect T[_] embedded in a tree of possible effects R
 *
 * The effect tree is represented by four possible cases:
 *   - fx1[T]
 *   - fx2[T1, T2]
 *   - fx3[T1, T2, T3]
 *   - FxAppend[L, R]
 *
 *  The union type has three concrete constructors:
 *   - UnionAppendL(nested: Union[L]): Union[FxAppend[L, R]]
 *   - UnionAppendR(nested: Union[R]): Union[FxAppend[L, R]]
 *   - UnionTagged(valueUnsafe: Any, index: Int): Union[R] (for R in fx1, fx2, fx3...)
 *  In that respect UnionTagged behaves similarly to a tagged union in C or C++.
 *
 */
sealed trait Effect[R, A] {
  type X = A
}

case class NoEffect[R, A](a: A) extends Effect[R, A]

sealed trait Union[R, A] extends Effect[R, A]  {
  final private[eff] def forget[E, B]: Union[E, B] =
    asInstanceOf[Union[E, B]]

  final private[eff] def tagged: UnionTagged[R, A] =
    this.asInstanceOf[UnionTagged[R, A]]
}

case class UnionTagged[R, A] (valueUnsafe: Any, index: Int) extends Union[R, A] {
  private[eff] def increment[E]: Union[E, A] = copy(index = index + 1)
  private[eff] def decrement[E]: Union[E, A] = copy(index = index - 1)
}
case class UnionAppendL[L, R, A](value: Union[L, A]) extends Union[FxAppend[L, R], A]
case class UnionAppendR[L, R, A](value: Union[R, A]) extends Union[FxAppend[L, R], A]

object Union {
  def one[M[_], A](value: M[A]): Union[Fx1[M], A] =
    UnionTagged(value, 1)

  def twoL[M[_], T[_], A](value: M[A]): Union[Fx2[M, T], A] =
    UnionTagged(value, 1)

  def twoR[M[_], T[_], A](value: T[A]): Union[Fx2[M, T], A] =
    UnionTagged(value, 2)

  def threeL[M[_], T[_], N[_], A](value: M[A]): Union[Fx3[M, T, N], A] =
    UnionTagged(value, 1)

  def threeM[M[_], T[_], N[_], A](value: T[A]): Union[Fx3[M, T, N], A] =
    UnionTagged(value, 2)

  def threeR[M[_], T[_], N[_], A](value: N[A]): Union[Fx3[M, T, N], A] =
    UnionTagged(value, 3)

  def appendL[L, R, A](union: Union[L, A]): Union[FxAppend[L, R], A] =
    UnionAppendL(union)

  def appendR[L, R, A](union: Union[R, A]): Union[FxAppend[L, R], A] =
    UnionAppendR(union)
}

