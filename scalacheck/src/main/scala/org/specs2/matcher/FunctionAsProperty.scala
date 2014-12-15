package org.specs2
package matcher

import org.scalacheck.{Prop, Shrink, Arbitrary}

/**
 * This trait adds some syntactic sugar to transform function
 * to properties by appending forAll
 */
trait FunctionAsProperty {
  /** transform a function returning a boolean to a property by appending forAll */
  implicit def functionToProp[T](f: T => Boolean)(implicit a: Arbitrary[T], s: Shrink[T]): Prop =
    FunctionToForAll(f).forAll

  implicit class FunctionToForAll[T](f: T => Boolean)(implicit a: Arbitrary[T], s: Shrink[T]) {
    def forAll: Prop = Prop.forAll(f)
  }

  /** transform a function returning a boolean to a property by appending forAll */
  implicit class FunctionToForAll2[T1, T2](f: (T1, T2) => Boolean) {
    def forAll(implicit
               a1: Arbitrary[T1], s1: Shrink[T1],
               a2: Arbitrary[T2], s2: Shrink[T2]
                ): Prop = Prop.forAll(f)
  }

  /** transform a function returning a boolean to a property by appending forAll */
  implicit class FunctionToForAll3[T1, T2, T3](f: (T1, T2, T3) => Boolean) {
    def forAll(implicit
               a1: Arbitrary[T1], s1: Shrink[T1],
               a2: Arbitrary[T2], s2: Shrink[T2],
               a3: Arbitrary[T3], s3: Shrink[T3]
                ): Prop = Prop.forAll(f)
  }
}

