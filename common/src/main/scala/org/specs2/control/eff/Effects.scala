package org.specs2.control.eff

/**
 * Effects is a type level representing a list of effects
 * which might take place in a computation
 *
 * Note that each "effect" is a type constructor
 */
trait Effects  {
  type |:[H[_], T <: Effects] = EffectsCons[H, T]

  val |: = EffectsCons
}

/** one effect, basically a type constructor */
sealed trait Effect[F[_]]

/**
 * Append an effect at the beginning of a list of effects
 */
final case class EffectsCons[F[_], T <: Effects](head: Effect[F], tail: T) extends Effects {
  def |:[G[_]](g: Effect[G]) = EffectsCons[G, F |: T](g, this)
}

/**
 * Nil case for the list of effects
 */
class NoEffect extends Effects {
  def |:[G[_]](g: Effect[G]) = EffectsCons[G, NoEffect](g, this)
}

object NoEffect extends NoEffect

/**
 * Aliases for declaring effects with the following syntax
 *
 *  A |: B |: C |: NoEffect
 *
 */
object Effects extends Effects


