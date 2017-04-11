package org.specs2
package scalacheck

import org.scalacheck.Gen
import org.specs2.fp._

/**
 * Scalaz instances for the Gen datatype
 */
trait GenInstances {

  implicit def genMonad: Monad[Gen] = new Monad[Gen] {
    def point[A](a: =>A): Gen[A] =
      Gen.const(a)

    def bind[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] =
      fa flatMap f
  }

}
