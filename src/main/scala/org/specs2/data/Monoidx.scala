package org.specs2.data

import org.specs2.internal.scalaz._
import Scalaz._

/**
 * This trait provides additional Monoid instances
 */
private[specs2]
trait Monoidx {
  /**
   * monoid instance for a Map where there is a Monoid for the key.
   *
   * Each time a key, value pair is added, the values are appended together
   */
  implicit def mapMonoid[K, V : Monoid]: Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    private val monoid = implicitly[Monoid[V]]

    lazy val zero = Map[K, V]().withDefaultValue(monoid.zero)

    def append(m1: Map[K, V], m2: =>Map[K, V]) = m2.foldLeft(m1) { (res, cur) =>
      res + (cur._1 -> (res.getOrElse(cur._1, monoid.zero) |+| cur._2))
    }
  }
}

private[specs2]
object Monoidx extends Monoidx