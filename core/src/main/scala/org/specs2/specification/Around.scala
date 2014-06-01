package org.specs2
package specification

import execute._
import java.io.PrintStream

import scalaz.Monad

/**
 * The Around trait can be inherited by classes which will
 * execute some code inside the around method provided by the context.
 * 
 * This can be used for example to execute some code inside a webapp session
 * 
 * @see Example to understand why the type T must : AsResult
 */
trait Around extends Context { outer =>

  def around[T : AsResult](t: =>T): Result
  def apply[T : AsResult](a: =>T) = around(a)
  
  /** compose the actions of 2 Around traits */
  def compose(a: Around): Around = new Around {
    def around[T : AsResult](t: =>T): Result = {
      a.around(outer.around(t))
    }
  }

  /** sequence the actions of 2 Around traits */
  def andThen(a: Around): Around = new Around {
    def around[T : AsResult](t: =>T): Result = {
      outer.around(a.around(t))
    }
  }
}

/**
 * A Fixture can be implicitly passed to a set of examples taking a function as an input.
 *
 * It can effectively act as an Outside and an Around context
 */
trait Fixture[T] {
  def apply[R : AsResult](f: T => R): Result
}

object Fixture {
  implicit def fixtureHasMonad: Monad[Fixture] = new Monad[Fixture] {
    def point[A](a: =>A) = new Fixture[A] {
      def apply[R : AsResult](f: A => R): Result = AsResult(f(a))
    }
    def bind[A, B](fixture: Fixture[A])(fa: A => Fixture[B]): Fixture[B] = new Fixture[B] {
      def apply[R : AsResult](fb: B => R): Result = fixture((a: A) => fa(a)(fb))
    }
  }
}

