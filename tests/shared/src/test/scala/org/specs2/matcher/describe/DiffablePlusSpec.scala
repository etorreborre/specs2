package org.specs2.matcher.describe

import org.specs2.Spec
import org.specs2.text.AnsiColors.*
import org.specs2.text.Whitespace.*

import scala.util.Failure

/** special cases which can not be executed with scala 2.10 */
class DiffablePlusSpec extends Spec {
  def is = s2"""

  Scala Objects: Either
  =====================

  Support Right without Left type information ${Diffable.diff(Right("abc"), Right("abc")) must ===(
    EitherIdentical(PrimitiveIdentical("abc"), isRight = true)
  )}
  Support Left without Right type information ${Diffable.diff(Left("abc"), Left("abc")) must ===(
    EitherIdentical(PrimitiveIdentical("abc"), isRight = false)
  )}


  Scala Objects: Try
  ==================

  Support failure with no type information ${Diffable.diff(Failure(ex), Failure(ex2)) must ===(
    TryDifferent(Diffable.diff(ex, ex2), isSuccess = false)
  )}

  Case classes
  ============
  case classes without any members are identical ${Diffable.diff(EmptyCaseClass(), EmptyCaseClass()).identical}
  two identical case classes return true ${Diffable.diff(Foo("a", 1), Foo("a", 1)).identical}
  two different case classes render their diff ${
    Diffable.diff(Foo("a", 1), Foo("b", 2)).render
    ===
    """|Foo(x: 'a' != 'b'
       |    y: 1 != 2)""".stripMargin
  }
  additional test case ${
    val actual = Book("Programming in Scala", Seq("Odersky", "me", "Venners"), 2008)
    val expected = Book("Programming in Scala", Seq("Odersky", "Spoon", "Venners"), 2009)
    Diffable.diff(actual, expected).render.removeColors.showSpaces ===
      """|Book(title: 'Programming in Scala'
         |     authors:
         |       Odersky
         |       - me
         |       + Spoon
         |       Venners
         |     pubYear: 2008 != 2009)""".stripMargin.showSpaces

  }



"""

  val ex = new RuntimeException
  val ex2 = new RuntimeException

}

case class EmptyCaseClass()

case class Foo(x: String, y: Int)
case class Bar(foo: Foo)

case class Book(title: String, authors: Seq[String], pubYear: Int)
