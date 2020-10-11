package org.specs2.matcher.describe

import org.specs2.Spec

import scala.util.Failure

/** special cases which can not be executed with scala 2.10 */
class DiffablePlusSpec extends Spec { def is = s2"""

  Scala Objects: Either
  =====================

  Support Right without Left type information ${
    Diffable.diff(Right("abc"), Right("abc")) must ===(EitherIdentical(PrimitiveIdentical("abc"), isRight = true))
  }
  Support Left without Right type information ${
    Diffable.diff(Left("abc"), Left("abc")) must ===(EitherIdentical(PrimitiveIdentical("abc"), isRight = false))
  }


  Scala Objects: Try
  ==================

  Support failure with no type information ${
    Diffable.diff(Failure(ex), Failure(ex2)) must ===(TryDifferent(Diffable.diff(ex, ex2), isSuccess = false))
  }

"""

  val ex = new RuntimeException
  val ex2 = new RuntimeException

}

case class EmptyCaseClass()
case class Foo(a: String, b: String, goo: Goo)
case class Goo(c: String, d: String)
