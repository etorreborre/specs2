package org.specs2.control.eff

import Effects._

/**
 * Effects is a type level representing a list of effects
 * which might take place in a computation
 *
 * Note that each "effect" is a type constructor
 */
trait Effects

/** one effect, basically a type constructor */
trait Effect[F[_]]

/**
 * Append an effect at the beginning of a list of effects
 */
final case class EffectsCons[F[_], T <: Effects](head: Effect[F], tail: T) extends Effects {
  def |:[G[_]](g: Effect[G]) = EffectsCons[G, F |: T](g, this)
}

/**
 * Nil case for the list of effects
 */
sealed class NoEffect extends Effects {
  def |:[G[_]](g: Effect[G]) = EffectsCons[G, NoEffect](g, this)
}

/**
 * Aliases for declaring effects with the following syntax
 *
 *  A |: B |: C |: NoEffect
 *
 */
object Effects {
  type |:[H[_], T <: Effects] = EffectsCons[H, T]

  val |: = EffectsCons
}


