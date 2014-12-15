package org.specs2
package matcher

import org.scalacheck.{Shrink, Prop, Arbitrary}

/**
 * This trait enables some syntactic sugar when it is necessary to pass several arbitrary instances
 */
trait ApplicableArbitraries { this: ScalaCheckMatchers =>
  implicit class ApplicableArbitrary[T](a: Arbitrary[T]) {
    def apply[R](f: T => R)(implicit toProp: (=>R) => Prop, s: Shrink[T]) = prop(f)(toProp, a, s)
  }

  implicit class ApplicableArbitrary2[T1, T2](a: (Arbitrary[T1], Arbitrary[T2])) {
    def apply[R](f: (T1, T2) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2]) = prop(f)(toProp, a._1, s1, a._2, s2)
  }

  implicit class ApplicableArbitrary3[T1, T2, T3](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3])) {
    def apply[R](f: (T1, T2, T3) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3]) = prop(f)(toProp, a._1, s1, a._2, s2, a._3, s3)
  }

  implicit class ApplicableArbitrary4[T1, T2, T3, T4](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3], Arbitrary[T4])) {
    def apply[R](f: (T1, T2, T3, T4) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4]) = prop(f)(toProp, a._1, s1, a._2, s2, a._3, s3, a._4, s4)
  }

  implicit class ApplicableArbitrary5[T1, T2, T3, T4, T5](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3], Arbitrary[T4], Arbitrary[T5])) {
    def apply[R](f: (T1, T2, T3, T4, T5) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4], s5: Shrink[T5]) = prop(f)(toProp, a._1, s1, a._2, s2, a._3, s3, a._4, s4, a._5, s5)
  }

  implicit class ApplicableArbitrary6[T1, T2, T3, T4, T5, T6](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3], Arbitrary[T4], Arbitrary[T5], Arbitrary[T6])) {
    def apply[R](f: (T1, T2, T3, T4, T5, T6) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4], s5: Shrink[T5], s6: Shrink[T6]) = prop(f)(toProp, a._1, s1, a._2, s2, a._3, s3, a._4, s4, a._5, s5, a._6, s6)
  }

  implicit class ApplicableArbitrary7[T1, T2, T3, T4, T5, T6, T7](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3], Arbitrary[T4], Arbitrary[T5], Arbitrary[T6], Arbitrary[T7])) {
    def apply[R](f: (T1, T2, T3, T4, T5, T6, T7) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4], s5: Shrink[T5], s6: Shrink[T6], s7: Shrink[T7]) = prop(f)(toProp, a._1, s1, a._2, s2, a._3, s3, a._4, s4, a._5, s5, a._6, s6, a._7, s7)
  }

  implicit class ApplicableArbitrary8[T1, T2, T3, T4, T5, T6, T7, T8](a: (Arbitrary[T1], Arbitrary[T2], Arbitrary[T3], Arbitrary[T4], Arbitrary[T5], Arbitrary[T6], Arbitrary[T7], Arbitrary[T8])) {
    def apply[R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(implicit toProp: (=>R) => Prop, s1: Shrink[T1], s2: Shrink[T2], s3: Shrink[T3], s4: Shrink[T4], s5: Shrink[T5], s6: Shrink[T6], s7: Shrink[T7], s8: Shrink[T8]) = prop(f)(toProp, a._1, s1, a._2, s2, a._3, s3, a._4, s4, a._5, s5, a._6, s6, a._7, s7, a._8, s8)
  }

}


