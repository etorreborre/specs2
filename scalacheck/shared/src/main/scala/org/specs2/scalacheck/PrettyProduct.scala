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

  def toString[P <: Product](p: P): String =
    p.productIterator.map {
      case prod: Product => PrettyProduct.toString(p)
      case s: String     => "\""+s+"\""
      case other         => other.toString
    }.mkString(p.productPrefix+"(", ", ", ")")

  def apply[P <: Product] = (p: P) => Pretty(_ => toString(p))
