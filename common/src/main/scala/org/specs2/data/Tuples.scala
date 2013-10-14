package org.specs2
package data


/**
 * Utility methods for tuples to flatten 3-tuples and 4-tuples
 */
private[specs2]
trait Tuples { outer =>

  implicit def toFlattenedTuple3[T1, T2, T3](t: ((T1, T2), T3)) = new FlattenedTuple3(t)
  case class FlattenedTuple3[T1, T2, T3](t: ((T1, T2), T3)) {
    def flatten = outer.flatten(t)
  }
  implicit def toFlattenedTuple4[T1, T2, T3, T4](t: (((T1, T2), T3), T4)) = new FlattenedTuple4(t)
  case class FlattenedTuple4[T1, T2, T3, T4](t: (((T1, T2), T3), T4)) {
    def flatten = outer.flatten(t)
  }
  def flatten[T1, T2, T3](t: ((T1, T2), T3)): (T1, T2, T3) = (t._1._1, t._1._2, t._2)
  def flatten[T1, T2, T3, T4](t: (((T1, T2), T3), T4)): (T1, T2, T3, T4) = { 
    val f = flatten(t._1)
    (f._1, f._2, f._3, t._2)
  }
}
private[specs2]
object Tuples extends Tuples

trait TuplesToSeq {
  implicit def tupleToSeq2[T,T1,T2]                           (t: (T1,T2))                          (implicit e1:T1<:<T,e2:T2<:<T): Seq[T]                                                                       = Seq(t._1,t._2)
  implicit def tupleToSeq3[T,T1,T2,T3]                        (t: (T1,T2,T3))                       (implicit e1:T1<:<T,e2:T2<:<T,e3:T3<:<T): Seq[T]                                                             = Seq(t._1,t._2,t._3)
  implicit def tupleToSeq4[T,T1,T2,T3,T4]                     (t: (T1,T2,T3,T4))                    (implicit e1:T1<:<T,e2:T2<:<T,e3:T3<:<T,e4:T4<:<T): Seq[T]                                                   = Seq(t._1,t._2,t._3,t._4)
  implicit def tupleToSeq5[T,T1,T2,T3,T4,T5]                  (t: (T1,T2,T3,T4,T5))                 (implicit e1:T1<:<T,e2:T2<:<T,e3:T3<:<T,e4:T4<:<T,e5:T5<:<T): Seq[T]                                         = Seq(t._1,t._2,t._3,t._4,t._5)
  implicit def tupleToSeq6[T,T1,T2,T3,T4,T5,T6]               (t: (T1,T2,T3,T4,T5,T6))              (implicit e1:T1<:<T,e2:T2<:<T,e3:T3<:<T,e4:T4<:<T,e5:T5<:<T,e6:T6<:<T): Seq[T]                               = Seq(t._1,t._2,t._3,t._4,t._5,t._6)
  implicit def tupleToSeq7[T,T1,T2,T3,T4,T5,T6,T7]            (t: (T1,T2,T3,T4,T5,T6,T7))           (implicit e1:T1<:<T,e2:T2<:<T,e3:T3<:<T,e4:T4<:<T,e5:T5<:<T,e6:T6<:<T,e7:T7<:<T): Seq[T]                     = Seq(t._1,t._2,t._3,t._4,t._5,t._6,t._7)
  implicit def tupleToSeq8[T,T1,T2,T3,T4,T5,T6,T7,T8]         (t: (T1,T2,T3,T4,T5,T6,T7,T8))        (implicit e1:T1<:<T,e2:T2<:<T,e3:T3<:<T,e4:T4<:<T,e5:T5<:<T,e6:T6<:<T,e7:T7<:<T,e8:T8<:<T): Seq[T]           = Seq(t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8)
  implicit def tupleToSeq9[T,T1,T2,T3,T4,T5,T6,T7,T8,T9]      (t: (T1,T2,T3,T4,T5,T6,T7,T8,T9))     (implicit e1:T1<:<T,e2:T2<:<T,e3:T3<:<T,e4:T4<:<T,e5:T5<:<T,e6:T6<:<T,e7:T7<:<T,e8:T8<:<T,e9:T9<:<T): Seq[T] = Seq(t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9)
  implicit def tupleToSeq10[T,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10] (t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10)) (implicit e1:T1<:<T,e2:T2<:<T,e3:T3<:<T,e4:T4<:<T,e5:T5<:<T,e6:T6<:<T,e7:T7<:<T,e8:T8<:<T,e9:T9<:<T,e10:T10<:<T): Seq[T] = Seq(t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10)
}
object TuplesToSeq extends TuplesToSeq

trait NoTuplesToSeq extends TuplesToSeq {
  override def tupleToSeq2[T,T1,T2]                           (t: (T1,T2))                          (implicit e1:T1<:<T,e2:T2<:<T): Seq[T]                                                                       = super.tupleToSeq2(t)
  override def tupleToSeq3[T,T1,T2,T3]                        (t: (T1,T2,T3))                       (implicit e1:T1<:<T,e2:T2<:<T,e3:T3<:<T): Seq[T]                                                             = super.tupleToSeq3(t)
  override def tupleToSeq4[T,T1,T2,T3,T4]                     (t: (T1,T2,T3,T4))                    (implicit e1:T1<:<T,e2:T2<:<T,e3:T3<:<T,e4:T4<:<T): Seq[T]                                                   = super.tupleToSeq4(t)
  override def tupleToSeq5[T,T1,T2,T3,T4,T5]                  (t: (T1,T2,T3,T4,T5))                 (implicit e1:T1<:<T,e2:T2<:<T,e3:T3<:<T,e4:T4<:<T,e5:T5<:<T): Seq[T]                                         = super.tupleToSeq5(t)
  override def tupleToSeq6[T,T1,T2,T3,T4,T5,T6]               (t: (T1,T2,T3,T4,T5,T6))              (implicit e1:T1<:<T,e2:T2<:<T,e3:T3<:<T,e4:T4<:<T,e5:T5<:<T,e6:T6<:<T): Seq[T]                               = super.tupleToSeq6(t)
  override def tupleToSeq7[T,T1,T2,T3,T4,T5,T6,T7]            (t: (T1,T2,T3,T4,T5,T6,T7))           (implicit e1:T1<:<T,e2:T2<:<T,e3:T3<:<T,e4:T4<:<T,e5:T5<:<T,e6:T6<:<T,e7:T7<:<T): Seq[T]                     = super.tupleToSeq7(t)
  override def tupleToSeq8[T,T1,T2,T3,T4,T5,T6,T7,T8]         (t: (T1,T2,T3,T4,T5,T6,T7,T8))        (implicit e1:T1<:<T,e2:T2<:<T,e3:T3<:<T,e4:T4<:<T,e5:T5<:<T,e6:T6<:<T,e7:T7<:<T,e8:T8<:<T): Seq[T]           = super.tupleToSeq8(t)
  override def tupleToSeq9[T,T1,T2,T3,T4,T5,T6,T7,T8,T9]      (t: (T1,T2,T3,T4,T5,T6,T7,T8,T9))     (implicit e1:T1<:<T,e2:T2<:<T,e3:T3<:<T,e4:T4<:<T,e5:T5<:<T,e6:T6<:<T,e7:T7<:<T,e8:T8<:<T,e9:T9<:<T): Seq[T] = super.tupleToSeq9(t)
  override def tupleToSeq10[T,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10] (t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10)) (implicit e1:T1<:<T,e2:T2<:<T,e3:T3<:<T,e4:T4<:<T,e5:T5<:<T,e6:T6<:<T,e7:T7<:<T,e8:T8<:<T,e9:T9<:<T,e10:T10<:<T): Seq[T] = super.tupleToSeq10(t)
}
