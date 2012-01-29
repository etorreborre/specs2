package org.specs2
package data

/**
 * Utility methods for tuples to flatten 3-tuples and 4-tuples
 */
private[specs2]
trait Tuples { outer =>

  implicit def toFlattenedTuple3[T1, T2, T3](t: ((T1, T2), T3)) = new FlattenedTuple3(t)
  case class FlattenedTuple3[T1, T2, T3](t: ((T1, T2), T3)) {
    def flatten = outer.flatten(t)
  }
  implicit def toFlattenedTuple4[T1, T2, T3, T4](t: (((T1, T2), T3), T4)) = new FlattenedTuple4(t)
  case class FlattenedTuple4[T1, T2, T3, T4](t: (((T1, T2), T3), T4)) {
    def flatten = outer.flatten(t)
  }
  def flatten[T1, T2, T3](t: ((T1, T2), T3)): (T1, T2, T3) = (t._1._1, t._1._2, t._2)
  def flatten[T1, T2, T3, T4](t: (((T1, T2), T3), T4)): (T1, T2, T3, T4) = {
    val f = flatten(t._1)
    (f._1, f._2, f._3, t._2)
  }
}
private[specs2]
object Tuples extends Tuples
