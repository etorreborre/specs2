package org.specs2
package fp


trait Identityx:
  /**
   * These extension methods provide the possibility to execute a function on an object if a condition is true
   * If not, the object is returned
   */
  extension [T](t: =>T)
    def orWhen(condition: Boolean)(f: T => T): T =
      if condition then f(t) else t

  extension [S <: T, T : Monoid](t: =>S)
    def orEmptyUnless(condition: Boolean): T =
      if condition then t else summon[Monoid[T]].zero

    def orEmptyWhen(condition: Boolean): T =
      if condition then t else summon[Monoid[T]].zero

object Identityx extends Identityx
