package org.specs2
package scalacheck

import org.scalacheck.Gen
import org.specs2.fp.*

/**
 * Scalaz instances for the Gen datatype
 */
trait GenInstances:

  given Monad[Gen] with
    def point[A](a: =>A): Gen[A] =
      Gen.const(a)

    def bind[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] =
      fa `flatMap` f
