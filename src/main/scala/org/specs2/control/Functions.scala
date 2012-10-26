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
}

object Functions extends Functions
