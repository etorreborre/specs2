package org.specs2.control.eff

trait Evaluated[+T] {
  def value: T
}

case class Memoized[T](t: () => T) extends Evaluated[T] {
  lazy val result: T = t()
  def value: T = result
}

object Memoized {
  def apply[T](t: =>T): Evaluated[T] =
    Memoized(() => t)
}

object Now {
  def apply[T](t: T): Evaluated[T] =
    Memoized(() => t)
}

