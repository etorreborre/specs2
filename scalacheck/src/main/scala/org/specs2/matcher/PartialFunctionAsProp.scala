package org.specs2
package matcher

import org.scalacheck.{Shrink, Arbitrary, Prop}

/**
 * This trait adds some syntactic sugar to transform partial functions to properties by appending forAll
 */
trait PartialFunctionAsProp {

  /** transform a partial function returning a boolean to a property by appending forAll */
  implicit class PartialFunctionForAll[T, S](f: PartialFunction[T, S]) {
    def forAll(implicit toProp: S => Prop, a: Arbitrary[T], s: Shrink[T]): Prop = Prop.forAll(f)
    def forAllNoShrink(implicit toProp: S => Prop, a: Arbitrary[T]): Prop = Prop.forAllNoShrink(a.arbitrary)(f)
  }
}

object PartialFunctionAsProp extends PartialFunctionAsProp

