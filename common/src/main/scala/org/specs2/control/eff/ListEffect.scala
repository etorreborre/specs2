package org.specs2.control.eff

import Eff._
import Effects._
import Interpret._
import Member._

import scala.collection.mutable.ListBuffer
import scalaz.{-\/, \/, \/-}

/**
 * Effect for computations possibly returning several values
 */
object ListEffect {

  /** create a list effect from a single value */
  def singleton[R, A](a: A)(implicit m: List <= R): Eff[R, A] =
    fromList(List(a))

  /** create a list effect from a list of values */
  def values[R, A](as: A*)(implicit m: List <= R): Eff[R, A] =
    fromList(as.toList)

  /** create a list effect from a list of values */
  def fromList[R, A](as: List[A])(implicit m: List <= R): Eff[R, A] =
    send[List, R, A](as)

  /** run an effect stack starting with a list effect */
  def runList[R <: Effects, A](effects: Eff[List |: R, A]): Eff[R, List[A]] = {
    val loop = new Loop[List, R, A, Eff[R, List[A]]] {
      type S = (List[Eff[List |: R, A]], ListBuffer[A])
      val init = (List[Eff[List |: R, A]](), new ListBuffer[A])

      def onPure(a: A, s: S): (Eff[List |: R, A], S) \/ Eff[R, List[A]] =
        s match {
          case (head :: tail, result) => -\/((head, (tail, result :+ a)))
          case (List(), result)       => \/-(EffMonad[R].point((result :+ a).toList))
        }

      def onEffect[X](l: List[X], continuation: Arrs[List |: R, X, A], s: S): (Eff[List |: R, A], S) \/ Eff[R, List[A]] =
        (l, s) match {
          case (List(), (head :: tail, result)) =>
            -\/((head, (tail, result)))

          case (List(), (List(), result)) =>
            \/-(EffMonad[R].point(result.toList))

          case (head :: tail, (unevaluated, result)) =>
            -\/((continuation(head), (tail.asInstanceOf[List[A]].map(a => continuation(a.asInstanceOf[X])) ++ unevaluated, result)))
        }
    }

    interpretLoop1((a: A) => List(a))(loop)(effects)
  }
}
