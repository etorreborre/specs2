package org.specs2
package scalacheck

import org.scalacheck.util.Pretty

/**
 * This object can be used to create Pretty instances for case class
 * Where string attributes are being quoted for easier copy/pasting from the console
 * when it is needed to replay a failing example. Usage:
 *
 *   case class MyInt(i: Int, s: String = "hey")
 *
 *   object MyInt {
 *     given Arbitrary[MyInt] = Arbitrary(Gen.const(MyInt(1)))
 *     given MyInt => Pretty = PrettyProduct[MyInt]
 *   }
 *
 */
object PrettyProduct:

  def toString[P <: Product](ps: P): String =
    ps.productIterator.map { p =>
      p.asInstanceOf[Matchable] match
        case prod: Product => PrettyProduct.toString(ps)
        case s: String     => "\""+s+"\""
        case other         => other.toString
    }.mkString(ps.productPrefix+"(", ", ", ")")

  def apply[P <: Product] = (p: P) => Pretty(_ => toString(p))
