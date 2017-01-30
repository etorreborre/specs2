package org.specs2.matcher
package describe

import org.specs2.Spec
import org.specs2.matcher._

import scala.util.Failure
import CaseClassDiffs._

/** special cases which can not be executed with scala 2.10 */
class DiffablePlusSpec extends Spec { def is = s2"""

  Case classes
  ============
    case classes without any members will return CaseClassIdentical ${ Diffable.diff(EmptyCaseClass(), EmptyCaseClass()) must_== CaseClassIdentical("EmptyCaseClass") }
    two identical case classes will return CaseClassIdentical       ${ Diffable.diff(Goo("a", "b"), Goo("a", "b")) must_== CaseClassIdentical("Goo") }
    two different case classes will return CaseClassDifferent       ${ Diffable.diff(Goo(c = "a", d = "b"), Goo(c = "x", d = "b")) must_== CaseClassDifferent("Goo", Seq(CaseClassPropertyComparison("c", PrimitiveDifference("a", "x"), identical = false), CaseClassPropertyComparison("d", PrimitiveIdentical("b"), identical = true))) }

  Scala Objects: Either
  =====================

  Support Right without Left type information       ${ Diffable.diff(Right("abc"), Right("abc")) must_=== EitherIdentical(PrimitiveIdentical("abc"), isRight = true) }
  Support Left without Right type information     ${ Diffable.diff(Left("abc"), Left("abc")) must_=== EitherIdentical(PrimitiveIdentical("abc"), isRight = false)  }


  Scala Objects: Try
  ==================

  Support failure with no type information                      ${ Diffable.diff(Failure(ex), Failure(ex2)) must_=== TryDifferent(Diffable.diff(ex, ex2), isSuccess = false) }

"""

  val ex = new RuntimeException
  val ex2 = new RuntimeException

}

case class EmptyCaseClass()
case class Foo(a: String, b: String, goo: Goo)
case class Goo(c: String, d: String)


