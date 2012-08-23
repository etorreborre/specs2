package org.specs2
package control

trait Functions {
  /** transform a byname Function1 into a strict Function1 */
  implicit def toStrictFunction1[T, S](f: (=>T) => S): T => S = (t: T) => f(t)
  /** transform a strict Function1 into a byname Function1 */
  implicit def toByNameFunction1[T, S](f: T => S): (=>T) => S = byName(_, f)

  private def byName[T, S](t: =>T, f: T => S) = f(t)
}

object Functions extends Functions
