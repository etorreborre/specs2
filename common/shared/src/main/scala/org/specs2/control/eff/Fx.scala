package org.specs2.control.eff

/**
 * Base type for a tree of effect types
 */
sealed trait Fx

object Fx {

  /**
   * Predefined type aliases to create effects
   */

  // prepend just one effect to a tree of effects
  type prepend[T[_], R] = FxAppend[Fx1[T], R]

  // append 2 trees of effects
  type append[L, R] = FxAppend[L, R]

  type fx1[T1[_]] = Fx1[T1]
  type fx2[T1[_], T2[_]] = Fx2[T1, T2]
  type fx3[T1[_], T2[_], T3[_]] = Fx3[T1, T2, T3]
  type fx4[T1[_], T2[_], T3[_], T4[_]] = FxAppend[Fx1[T1], Fx3[T2, T3, T4]]
  type fx5[T1[_], T2[_], T3[_], T4[_], T5[_]] = FxAppend[Fx2[T1, T2], Fx3[T3, T4, T5]]
  type fx6[T1[_], T2[_], T3[_], T4[_], T5[_], T6[_]] = FxAppend[Fx3[T1, T2, T3], Fx3[T4, T5, T6]]
  type fx7[T1[_], T2[_], T3[_], T4[_], T5[_], T6[_], T7[_]] = FxAppend[Fx1[T1], FxAppend[Fx3[T2, T3, T4], Fx3[T5, T6, T7]]]
  type fx8[T1[_], T2[_], T3[_], T4[_], T5[_], T6[_], T7[_], T8[_]] = FxAppend[Fx2[T1, T2], FxAppend[Fx3[T3, T4, T5], Fx3[T6, T7, T8]]]
  type fx9[T1[_], T2[_], T3[_], T4[_], T5[_], T6[_], T7[_], T8[_], T9[_]] = FxAppend[Fx3[T1, T2, T3], FxAppend[Fx3[T4, T5, T6], Fx3[T7, T8, T9]]]
  type fx10[T1[_], T2[_], T3[_], T4[_], T5[_], T6[_], T7[_], T8[_], T9[_], T10[_]] = FxAppend[FxAppend[Fx1[T1], Fx3[T2, T3, T4]], FxAppend[Fx3[T5, T6, T7], Fx3[T8, T9, T10]]]

  type fx11[T1[_], T2[_], T3[_], T4[_], T5[_], T6[_], T7[_], T8[_], T9[_], T10[_], T11[_]] = FxAppend[FxAppend[Fx2[T1, T2], Fx3[T3, T4, T5]], FxAppend[Fx3[T6, T7, T8], Fx3[T9, T10, T11]]]
  type fx12[T1[_], T2[_], T3[_], T4[_], T5[_], T6[_], T7[_], T8[_], T9[_], T10[_], T11[_], T12[_]] = FxAppend[FxAppend[Fx3[T1, T2, T3], Fx3[T4, T5, T6]], FxAppend[Fx3[T7, T8, T9], Fx3[T10, T11, T12]]]

}

/**
 * Append a  tree of effects to another one
 */
final case class FxAppend[L, R](left: L, right: R) extends Fx

trait Fx1[+F[_]] extends Fx
trait Fx2[+L[_], +R[_]] extends Fx
trait Fx3[+L[_], +M[_], +R[_]] extends Fx

/**
 * The "empty" tree of effects
 */
class NoFx extends Fx

object NoFx extends NoFx
