package org.specs2
package control

/**
 * This trait provides utility methods for functions
 */
trait Functions {
  /** transform a byname Function1 into a strict Function1 */
  implicit def toStrictFunction1[T, S](f: (=>T) => S): T => S = (t: T) => f(t)

  implicit def logicalFunction[A](f: A => Boolean): LogicalFunction[A] = LogicalFunction(f)

  case class LogicalFunction[A](f: A => Boolean) {
    def ||(g: A => Boolean) = (a: A) => f(a) || g(a)
    def &&(g: A => Boolean) = (a: A) => f(a) && g(a)
    def unary_!             = (a: A) => !f(a)
  }

  /** missing untupled functions to 10 arguments */
  def untupled[a1,a2,a3,a4,a5,a6,b](f: Tuple6[a1,a2,a3,a4,a5,a6] => b): (a1,a2,a3,a4,a5,a6) => b = (x1,x2,x3,x4,x5,x6) => f(Tuple6(x1,x2,x3,x4,x5,x6))
  def untupled[a1,a2,a3,a4,a5,a6,a7,b](f: Tuple7 [a1,a2,a3,a4,a5,a6,a7] => b): (a1,a2,a3,a4,a5,a6,a7) => b = (x1,x2,x3,x4,x5,x6,x7) => f(Tuple7 (x1,x2,x3,x4,x5,x6,x7))
  def untupled[a1,a2,a3,a4,a5,a6,a7,a8,b](f: Tuple8 [a1,a2,a3,a4,a5,a6,a7,a8] => b): (a1,a2,a3,a4,a5,a6,a7,a8) => b = (x1,x2,x3,x4,x5,x6,x7,x8) => f(Tuple8 (x1,x2,x3,x4,x5,x6,x7,x8))
  def untupled[a1,a2,a3,a4,a5,a6,a7,a8,a9,b](f: Tuple9 [a1,a2,a3,a4,a5,a6,a7,a8,a9] => b): (a1,a2,a3,a4,a5,a6,a7,a8,a9) => b = (x1,x2,x3,x4,x5,x6,x7,x8,x9) => f(Tuple9 (x1,x2,x3,x4,x5,x6,x7,x8,x9))
  def untupled[a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,b](f: Tuple10[a1,a2,a3,a4,a5,a6,a7,a8,a9,a10] => b): (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) => b = (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) => f(Tuple10(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))

  /** zip 2 state-folding functions together */
  implicit class zipFoldFunctions[T, S1](f1: (T, S1) => S1) {
    def zip[S2](f2: (T, S2) => S2): (T, (S1, S2)) => (S1, S2) = { (t: T, s12: (S1, S2)) =>
      val (s1, s2) = s12
      (f1(t, s1), f2(t, s2))
    }
  }

}

object Functions extends Functions
