package org.specs2
package matcher

import MatchersImplicits._

/**
 * This trait provides 'zip' operators to create matchers on tuples based on "zipped" matchers on fields
 */
trait MatcherZipOperators extends ExpectationsCreation { outer =>

  def contain[T, S](f: (=>(T)) => Matcher[S])(expected: =>Seq[T]) = (s: Seq[S]) =>
    expected.contain(f)(createExpectable(s))

  implicit class ContainSeqMatcherFunction[T](seq: Seq[T]) {
    def contain[S](f: (=>T) => Matcher[S]): ContainWithResultSeq[S] =
      new ContainWithResultSeq(seq.map(t => ValueChecks.matcherIsValueCheck(f(t)))).exactly
  }

  def zip[T1,T2, S1,S2](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2]):
  (=>(T1,T2)) => Matcher[(S1,S2)] = {

    def zip1(expected: =>(T1,T2)) = ((actual: (S1,S2)) => {
      val r = matchField(m1, expected._1, actual._1, "_1") and matchField(m2, expected._2, actual._2, "_2")
      (r.isSuccess, "For "+expected+"\n"+r.message)
    }): Matcher[(S1,S2)]
    zip1
  }

  implicit class TupleMatcher2[T1,T2](t: (T1,T2)) {
    def zip[S1,S2](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2]): Matcher[(S1,S2)] =
      outer.zip(m1,m2)(t)
  }


  def zip[T1,T2,T3, S1,S2,S3](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3]):
  (=>(T1,T2,T3)) => Matcher[(S1,S2,S3)] = {

    def zip1(expected: =>(T1,T2,T3)) = ((actual: (S1,S2,S3)) => {
      val r = matchField(m1, expected._1, actual._1, "_1") and matchField(m2, expected._2, actual._2, "_2") and matchField(m3, expected._3, actual._3, "_3")
      (r.isSuccess, "For "+expected+"\n"+r.message)
    }): Matcher[(S1,S2,S3)]
    zip1
  }

  implicit class TupleMatcher3[T1,T2,T3](t: (T1,T2,T3)) {
    def zip[S1,S2,S3](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3]): Matcher[(S1,S2,S3)] =
      outer.zip(m1,m2,m3)(t)
  }


  def zip[T1,T2,T3,T4, S1,S2,S3,S4](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4]):
  (=>(T1,T2,T3,T4)) => Matcher[(S1,S2,S3,S4)] = {

    def zip1(expected: =>(T1,T2,T3,T4)) = ((actual: (S1,S2,S3,S4)) => {
      val r = matchField(m1, expected._1, actual._1, "_1") and matchField(m2, expected._2, actual._2, "_2") and matchField(m3, expected._3, actual._3, "_3") and matchField(m4, expected._4, actual._4, "_4")
      (r.isSuccess, "For "+expected+"\n"+r.message)
    }): Matcher[(S1,S2,S3,S4)]
    zip1
  }

  implicit class TupleMatcher4[T1,T2,T3,T4](t: (T1,T2,T3,T4)) {
    def zip[S1,S2,S3,S4](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4]): Matcher[(S1,S2,S3,S4)] =
      outer.zip(m1,m2,m3,m4)(t)
  }


  def zip[T1,T2,T3,T4,T5, S1,S2,S3,S4,S5](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5]):
  (=>(T1,T2,T3,T4,T5)) => Matcher[(S1,S2,S3,S4,S5)] = {

    def zip1(expected: =>(T1,T2,T3,T4,T5)) = ((actual: (S1,S2,S3,S4,S5)) => {
      val r = matchField(m1, expected._1, actual._1, "_1") and matchField(m2, expected._2, actual._2, "_2") and matchField(m3, expected._3, actual._3, "_3") and matchField(m4, expected._4, actual._4, "_4") and matchField(m5, expected._5, actual._5, "_5")
      (r.isSuccess, "For "+expected+"\n"+r.message)
    }): Matcher[(S1,S2,S3,S4,S5)]
    zip1
  }

  implicit class TupleMatcher5[T1,T2,T3,T4,T5](t: (T1,T2,T3,T4,T5)) {
    def zip[S1,S2,S3,S4,S5](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5]): Matcher[(S1,S2,S3,S4,S5)] =
      outer.zip(m1,m2,m3,m4,m5)(t)
  }


  def zip[T1,T2,T3,T4,T5,T6, S1,S2,S3,S4,S5,S6](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6]):
  (=>(T1,T2,T3,T4,T5,T6)) => Matcher[(S1,S2,S3,S4,S5,S6)] = {

    def zip1(expected: =>(T1,T2,T3,T4,T5,T6)) = ((actual: (S1,S2,S3,S4,S5,S6)) => {
      val r = matchField(m1, expected._1, actual._1, "_1") and matchField(m2, expected._2, actual._2, "_2") and matchField(m3, expected._3, actual._3, "_3") and matchField(m4, expected._4, actual._4, "_4") and matchField(m5, expected._5, actual._5, "_5") and matchField(m6, expected._6, actual._6, "_6")
      (r.isSuccess, "For "+expected+"\n"+r.message)
    }): Matcher[(S1,S2,S3,S4,S5,S6)]
    zip1
  }

  implicit class TupleMatcher6[T1,T2,T3,T4,T5,T6](t: (T1,T2,T3,T4,T5,T6)) {
    def zip[S1,S2,S3,S4,S5,S6](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6]): Matcher[(S1,S2,S3,S4,S5,S6)] =
      outer.zip(m1,m2,m3,m4,m5,m6)(t)
  }


  def zip[T1,T2,T3,T4,T5,T6,T7, S1,S2,S3,S4,S5,S6,S7](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7]):
  (=>(T1,T2,T3,T4,T5,T6,T7)) => Matcher[(S1,S2,S3,S4,S5,S6,S7)] = {

    def zip1(expected: =>(T1,T2,T3,T4,T5,T6,T7)) = ((actual: (S1,S2,S3,S4,S5,S6,S7)) => {
      val r = matchField(m1, expected._1, actual._1, "_1") and matchField(m2, expected._2, actual._2, "_2") and matchField(m3, expected._3, actual._3, "_3") and matchField(m4, expected._4, actual._4, "_4") and matchField(m5, expected._5, actual._5, "_5") and matchField(m6, expected._6, actual._6, "_6") and matchField(m7, expected._7, actual._7, "_7")
      (r.isSuccess, "For "+expected+"\n"+r.message)
    }): Matcher[(S1,S2,S3,S4,S5,S6,S7)]
    zip1
  }

  implicit class TupleMatcher7[T1,T2,T3,T4,T5,T6,T7](t: (T1,T2,T3,T4,T5,T6,T7)) {
    def zip[S1,S2,S3,S4,S5,S6,S7](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7]): Matcher[(S1,S2,S3,S4,S5,S6,S7)] =
      outer.zip(m1,m2,m3,m4,m5,m6,m7)(t)
  }


  def zip[T1,T2,T3,T4,T5,T6,T7,T8, S1,S2,S3,S4,S5,S6,S7,S8](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8]):
  (=>(T1,T2,T3,T4,T5,T6,T7,T8)) => Matcher[(S1,S2,S3,S4,S5,S6,S7,S8)] = {

    def zip1(expected: =>(T1,T2,T3,T4,T5,T6,T7,T8)) = ((actual: (S1,S2,S3,S4,S5,S6,S7,S8)) => {
      val r = matchField(m1, expected._1, actual._1, "_1") and matchField(m2, expected._2, actual._2, "_2") and matchField(m3, expected._3, actual._3, "_3") and matchField(m4, expected._4, actual._4, "_4") and matchField(m5, expected._5, actual._5, "_5") and matchField(m6, expected._6, actual._6, "_6") and matchField(m7, expected._7, actual._7, "_7") and matchField(m8, expected._8, actual._8, "_8")
      (r.isSuccess, "For "+expected+"\n"+r.message)
    }): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8)]
    zip1
  }

  implicit class TupleMatcher8[T1,T2,T3,T4,T5,T6,T7,T8](t: (T1,T2,T3,T4,T5,T6,T7,T8)) {
    def zip[S1,S2,S3,S4,S5,S6,S7,S8](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8]): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8)] =
      outer.zip(m1,m2,m3,m4,m5,m6,m7,m8)(t)
  }


  def zip[T1,T2,T3,T4,T5,T6,T7,T8,T9, S1,S2,S3,S4,S5,S6,S7,S8,S9](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9]):
  (=>(T1,T2,T3,T4,T5,T6,T7,T8,T9)) => Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9)] = {

    def zip1(expected: =>(T1,T2,T3,T4,T5,T6,T7,T8,T9)) = ((actual: (S1,S2,S3,S4,S5,S6,S7,S8,S9)) => {
      val r = matchField(m1, expected._1, actual._1, "_1") and matchField(m2, expected._2, actual._2, "_2") and matchField(m3, expected._3, actual._3, "_3") and matchField(m4, expected._4, actual._4, "_4") and matchField(m5, expected._5, actual._5, "_5") and matchField(m6, expected._6, actual._6, "_6") and matchField(m7, expected._7, actual._7, "_7") and matchField(m8, expected._8, actual._8, "_8") and matchField(m9, expected._9, actual._9, "_9")
      (r.isSuccess, "For "+expected+"\n"+r.message)
    }): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9)]
    zip1
  }

  implicit class TupleMatcher9[T1,T2,T3,T4,T5,T6,T7,T8,T9](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9)) {
    def zip[S1,S2,S3,S4,S5,S6,S7,S8,S9](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9]): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9)] =
      outer.zip(m1,m2,m3,m4,m5,m6,m7,m8,m9)(t)
  }


  def zip[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10, S1,S2,S3,S4,S5,S6,S7,S8,S9,S10](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10]):
  (=>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10)) => Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10)] = {

    def zip1(expected: =>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10)) = ((actual: (S1,S2,S3,S4,S5,S6,S7,S8,S9,S10)) => {
      val r = matchField(m1, expected._1, actual._1, "_1") and matchField(m2, expected._2, actual._2, "_2") and matchField(m3, expected._3, actual._3, "_3") and matchField(m4, expected._4, actual._4, "_4") and matchField(m5, expected._5, actual._5, "_5") and matchField(m6, expected._6, actual._6, "_6") and matchField(m7, expected._7, actual._7, "_7") and matchField(m8, expected._8, actual._8, "_8") and matchField(m9, expected._9, actual._9, "_9") and matchField(m10, expected._10, actual._10, "_10")
      (r.isSuccess, "For "+expected+"\n"+r.message)
    }): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10)]
    zip1
  }

  implicit class TupleMatcher10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10)) {
    def zip[S1,S2,S3,S4,S5,S6,S7,S8,S9,S10](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10]): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10)] =
      outer.zip(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)(t)
  }


  def zip[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11, S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11]):
  (=>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11)) => Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11)] = {

    def zip1(expected: =>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11)) = ((actual: (S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11)) => {
      val r = matchField(m1, expected._1, actual._1, "_1") and matchField(m2, expected._2, actual._2, "_2") and matchField(m3, expected._3, actual._3, "_3") and matchField(m4, expected._4, actual._4, "_4") and matchField(m5, expected._5, actual._5, "_5") and matchField(m6, expected._6, actual._6, "_6") and matchField(m7, expected._7, actual._7, "_7") and matchField(m8, expected._8, actual._8, "_8") and matchField(m9, expected._9, actual._9, "_9") and matchField(m10, expected._10, actual._10, "_10") and matchField(m11, expected._11, actual._11, "_11")
      (r.isSuccess, "For "+expected+"\n"+r.message)
    }): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11)]
    zip1
  }

  implicit class TupleMatcher11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11)) {
    def zip[S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11]): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11)] =
      outer.zip(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11)(t)
  }


  def zip[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12, S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11],m12: (=>T12) => Matcher[S12]):
  (=>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12)) => Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12)] = {

    def zip1(expected: =>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12)) = ((actual: (S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12)) => {
      val r = matchField(m1, expected._1, actual._1, "_1") and matchField(m2, expected._2, actual._2, "_2") and matchField(m3, expected._3, actual._3, "_3") and matchField(m4, expected._4, actual._4, "_4") and matchField(m5, expected._5, actual._5, "_5") and matchField(m6, expected._6, actual._6, "_6") and matchField(m7, expected._7, actual._7, "_7") and matchField(m8, expected._8, actual._8, "_8") and matchField(m9, expected._9, actual._9, "_9") and matchField(m10, expected._10, actual._10, "_10") and matchField(m11, expected._11, actual._11, "_11") and matchField(m12, expected._12, actual._12, "_12")
      (r.isSuccess, "For "+expected+"\n"+r.message)
    }): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12)]
    zip1
  }

  implicit class TupleMatcher12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12)) {
    def zip[S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11],m12: (=>T12) => Matcher[S12]): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12)] =
      outer.zip(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12)(t)
  }


  def zip[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13, S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11],m12: (=>T12) => Matcher[S12],m13: (=>T13) => Matcher[S13]):
  (=>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13)) => Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13)] = {

    def zip1(expected: =>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13)) = ((actual: (S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13)) => {
      val r = matchField(m1, expected._1, actual._1, "_1") and matchField(m2, expected._2, actual._2, "_2") and matchField(m3, expected._3, actual._3, "_3") and matchField(m4, expected._4, actual._4, "_4") and matchField(m5, expected._5, actual._5, "_5") and matchField(m6, expected._6, actual._6, "_6") and matchField(m7, expected._7, actual._7, "_7") and matchField(m8, expected._8, actual._8, "_8") and matchField(m9, expected._9, actual._9, "_9") and matchField(m10, expected._10, actual._10, "_10") and matchField(m11, expected._11, actual._11, "_11") and matchField(m12, expected._12, actual._12, "_12") and matchField(m13, expected._13, actual._13, "_13")
      (r.isSuccess, "For "+expected+"\n"+r.message)
    }): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13)]
    zip1
  }

  implicit class TupleMatcher13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13)) {
    def zip[S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11],m12: (=>T12) => Matcher[S12],m13: (=>T13) => Matcher[S13]): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13)] =
      outer.zip(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13)(t)
  }


  def zip[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14, S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11],m12: (=>T12) => Matcher[S12],m13: (=>T13) => Matcher[S13],m14: (=>T14) => Matcher[S14]):
  (=>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14)) => Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14)] = {

    def zip1(expected: =>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14)) = ((actual: (S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14)) => {
      val r = matchField(m1, expected._1, actual._1, "_1") and matchField(m2, expected._2, actual._2, "_2") and matchField(m3, expected._3, actual._3, "_3") and matchField(m4, expected._4, actual._4, "_4") and matchField(m5, expected._5, actual._5, "_5") and matchField(m6, expected._6, actual._6, "_6") and matchField(m7, expected._7, actual._7, "_7") and matchField(m8, expected._8, actual._8, "_8") and matchField(m9, expected._9, actual._9, "_9") and matchField(m10, expected._10, actual._10, "_10") and matchField(m11, expected._11, actual._11, "_11") and matchField(m12, expected._12, actual._12, "_12") and matchField(m13, expected._13, actual._13, "_13") and matchField(m14, expected._14, actual._14, "_14")
      (r.isSuccess, "For "+expected+"\n"+r.message)
    }): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14)]
    zip1
  }

  implicit class TupleMatcher14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14)) {
    def zip[S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11],m12: (=>T12) => Matcher[S12],m13: (=>T13) => Matcher[S13],m14: (=>T14) => Matcher[S14]): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14)] =
      outer.zip(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14)(t)
  }


  def zip[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15, S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11],m12: (=>T12) => Matcher[S12],m13: (=>T13) => Matcher[S13],m14: (=>T14) => Matcher[S14],m15: (=>T15) => Matcher[S15]):
  (=>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15)) => Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15)] = {

    def zip1(expected: =>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15)) = ((actual: (S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15)) => {
      val r = matchField(m1, expected._1, actual._1, "_1") and matchField(m2, expected._2, actual._2, "_2") and matchField(m3, expected._3, actual._3, "_3") and matchField(m4, expected._4, actual._4, "_4") and matchField(m5, expected._5, actual._5, "_5") and matchField(m6, expected._6, actual._6, "_6") and matchField(m7, expected._7, actual._7, "_7") and matchField(m8, expected._8, actual._8, "_8") and matchField(m9, expected._9, actual._9, "_9") and matchField(m10, expected._10, actual._10, "_10") and matchField(m11, expected._11, actual._11, "_11") and matchField(m12, expected._12, actual._12, "_12") and matchField(m13, expected._13, actual._13, "_13") and matchField(m14, expected._14, actual._14, "_14") and matchField(m15, expected._15, actual._15, "_15")
      (r.isSuccess, "For "+expected+"\n"+r.message)
    }): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15)]
    zip1
  }

  implicit class TupleMatcher15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15)) {
    def zip[S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11],m12: (=>T12) => Matcher[S12],m13: (=>T13) => Matcher[S13],m14: (=>T14) => Matcher[S14],m15: (=>T15) => Matcher[S15]): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15)] =
      outer.zip(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15)(t)
  }


  def zip[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16, S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11],m12: (=>T12) => Matcher[S12],m13: (=>T13) => Matcher[S13],m14: (=>T14) => Matcher[S14],m15: (=>T15) => Matcher[S15],m16: (=>T16) => Matcher[S16]):
  (=>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16)) => Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16)] = {

    def zip1(expected: =>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16)) = ((actual: (S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16)) => {
      val r = matchField(m1, expected._1, actual._1, "_1") and matchField(m2, expected._2, actual._2, "_2") and matchField(m3, expected._3, actual._3, "_3") and matchField(m4, expected._4, actual._4, "_4") and matchField(m5, expected._5, actual._5, "_5") and matchField(m6, expected._6, actual._6, "_6") and matchField(m7, expected._7, actual._7, "_7") and matchField(m8, expected._8, actual._8, "_8") and matchField(m9, expected._9, actual._9, "_9") and matchField(m10, expected._10, actual._10, "_10") and matchField(m11, expected._11, actual._11, "_11") and matchField(m12, expected._12, actual._12, "_12") and matchField(m13, expected._13, actual._13, "_13") and matchField(m14, expected._14, actual._14, "_14") and matchField(m15, expected._15, actual._15, "_15") and matchField(m16, expected._16, actual._16, "_16")
      (r.isSuccess, "For "+expected+"\n"+r.message)
    }): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16)]
    zip1
  }

  implicit class TupleMatcher16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16)) {
    def zip[S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11],m12: (=>T12) => Matcher[S12],m13: (=>T13) => Matcher[S13],m14: (=>T14) => Matcher[S14],m15: (=>T15) => Matcher[S15],m16: (=>T16) => Matcher[S16]): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16)] =
      outer.zip(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)(t)
  }


  def zip[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17, S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11],m12: (=>T12) => Matcher[S12],m13: (=>T13) => Matcher[S13],m14: (=>T14) => Matcher[S14],m15: (=>T15) => Matcher[S15],m16: (=>T16) => Matcher[S16],m17: (=>T17) => Matcher[S17]):
  (=>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17)) => Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17)] = {

    def zip1(expected: =>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17)) = ((actual: (S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17)) => {
      val r = matchField(m1, expected._1, actual._1, "_1") and matchField(m2, expected._2, actual._2, "_2") and matchField(m3, expected._3, actual._3, "_3") and matchField(m4, expected._4, actual._4, "_4") and matchField(m5, expected._5, actual._5, "_5") and matchField(m6, expected._6, actual._6, "_6") and matchField(m7, expected._7, actual._7, "_7") and matchField(m8, expected._8, actual._8, "_8") and matchField(m9, expected._9, actual._9, "_9") and matchField(m10, expected._10, actual._10, "_10") and matchField(m11, expected._11, actual._11, "_11") and matchField(m12, expected._12, actual._12, "_12") and matchField(m13, expected._13, actual._13, "_13") and matchField(m14, expected._14, actual._14, "_14") and matchField(m15, expected._15, actual._15, "_15") and matchField(m16, expected._16, actual._16, "_16") and matchField(m17, expected._17, actual._17, "_17")
      (r.isSuccess, "For "+expected+"\n"+r.message)
    }): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17)]
    zip1
  }

  implicit class TupleMatcher17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17)) {
    def zip[S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11],m12: (=>T12) => Matcher[S12],m13: (=>T13) => Matcher[S13],m14: (=>T14) => Matcher[S14],m15: (=>T15) => Matcher[S15],m16: (=>T16) => Matcher[S16],m17: (=>T17) => Matcher[S17]): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17)] =
      outer.zip(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17)(t)
  }


  def zip[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18, S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11],m12: (=>T12) => Matcher[S12],m13: (=>T13) => Matcher[S13],m14: (=>T14) => Matcher[S14],m15: (=>T15) => Matcher[S15],m16: (=>T16) => Matcher[S16],m17: (=>T17) => Matcher[S17],m18: (=>T18) => Matcher[S18]):
  (=>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18)) => Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18)] = {

    def zip1(expected: =>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18)) = ((actual: (S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18)) => {
      val r = matchField(m1, expected._1, actual._1, "_1") and matchField(m2, expected._2, actual._2, "_2") and matchField(m3, expected._3, actual._3, "_3") and matchField(m4, expected._4, actual._4, "_4") and matchField(m5, expected._5, actual._5, "_5") and matchField(m6, expected._6, actual._6, "_6") and matchField(m7, expected._7, actual._7, "_7") and matchField(m8, expected._8, actual._8, "_8") and matchField(m9, expected._9, actual._9, "_9") and matchField(m10, expected._10, actual._10, "_10") and matchField(m11, expected._11, actual._11, "_11") and matchField(m12, expected._12, actual._12, "_12") and matchField(m13, expected._13, actual._13, "_13") and matchField(m14, expected._14, actual._14, "_14") and matchField(m15, expected._15, actual._15, "_15") and matchField(m16, expected._16, actual._16, "_16") and matchField(m17, expected._17, actual._17, "_17") and matchField(m18, expected._18, actual._18, "_18")
      (r.isSuccess, "For "+expected+"\n"+r.message)
    }): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18)]
    zip1
  }

  implicit class TupleMatcher18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18)) {
    def zip[S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11],m12: (=>T12) => Matcher[S12],m13: (=>T13) => Matcher[S13],m14: (=>T14) => Matcher[S14],m15: (=>T15) => Matcher[S15],m16: (=>T16) => Matcher[S16],m17: (=>T17) => Matcher[S17],m18: (=>T18) => Matcher[S18]): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18)] =
      outer.zip(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18)(t)
  }


  def zip[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19, S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11],m12: (=>T12) => Matcher[S12],m13: (=>T13) => Matcher[S13],m14: (=>T14) => Matcher[S14],m15: (=>T15) => Matcher[S15],m16: (=>T16) => Matcher[S16],m17: (=>T17) => Matcher[S17],m18: (=>T18) => Matcher[S18],m19: (=>T19) => Matcher[S19]):
  (=>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19)) => Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19)] = {

    def zip1(expected: =>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19)) = ((actual: (S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19)) => {
      val r = matchField(m1, expected._1, actual._1, "_1") and matchField(m2, expected._2, actual._2, "_2") and matchField(m3, expected._3, actual._3, "_3") and matchField(m4, expected._4, actual._4, "_4") and matchField(m5, expected._5, actual._5, "_5") and matchField(m6, expected._6, actual._6, "_6") and matchField(m7, expected._7, actual._7, "_7") and matchField(m8, expected._8, actual._8, "_8") and matchField(m9, expected._9, actual._9, "_9") and matchField(m10, expected._10, actual._10, "_10") and matchField(m11, expected._11, actual._11, "_11") and matchField(m12, expected._12, actual._12, "_12") and matchField(m13, expected._13, actual._13, "_13") and matchField(m14, expected._14, actual._14, "_14") and matchField(m15, expected._15, actual._15, "_15") and matchField(m16, expected._16, actual._16, "_16") and matchField(m17, expected._17, actual._17, "_17") and matchField(m18, expected._18, actual._18, "_18") and matchField(m19, expected._19, actual._19, "_19")
      (r.isSuccess, "For "+expected+"\n"+r.message)
    }): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19)]
    zip1
  }

  implicit class TupleMatcher19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19)) {
    def zip[S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11],m12: (=>T12) => Matcher[S12],m13: (=>T13) => Matcher[S13],m14: (=>T14) => Matcher[S14],m15: (=>T15) => Matcher[S15],m16: (=>T16) => Matcher[S16],m17: (=>T17) => Matcher[S17],m18: (=>T18) => Matcher[S18],m19: (=>T19) => Matcher[S19]): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19)] =
      outer.zip(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19)(t)
  }


  def zip[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20, S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11],m12: (=>T12) => Matcher[S12],m13: (=>T13) => Matcher[S13],m14: (=>T14) => Matcher[S14],m15: (=>T15) => Matcher[S15],m16: (=>T16) => Matcher[S16],m17: (=>T17) => Matcher[S17],m18: (=>T18) => Matcher[S18],m19: (=>T19) => Matcher[S19],m20: (=>T20) => Matcher[S20]):
  (=>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20)) => Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20)] = {

    def zip1(expected: =>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20)) = ((actual: (S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20)) => {
      val r = matchField(m1, expected._1, actual._1, "_1") and matchField(m2, expected._2, actual._2, "_2") and matchField(m3, expected._3, actual._3, "_3") and matchField(m4, expected._4, actual._4, "_4") and matchField(m5, expected._5, actual._5, "_5") and matchField(m6, expected._6, actual._6, "_6") and matchField(m7, expected._7, actual._7, "_7") and matchField(m8, expected._8, actual._8, "_8") and matchField(m9, expected._9, actual._9, "_9") and matchField(m10, expected._10, actual._10, "_10") and matchField(m11, expected._11, actual._11, "_11") and matchField(m12, expected._12, actual._12, "_12") and matchField(m13, expected._13, actual._13, "_13") and matchField(m14, expected._14, actual._14, "_14") and matchField(m15, expected._15, actual._15, "_15") and matchField(m16, expected._16, actual._16, "_16") and matchField(m17, expected._17, actual._17, "_17") and matchField(m18, expected._18, actual._18, "_18") and matchField(m19, expected._19, actual._19, "_19") and matchField(m20, expected._20, actual._20, "_20")
      (r.isSuccess, "For "+expected+"\n"+r.message)
    }): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20)]
    zip1
  }

  implicit class TupleMatcher20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20)) {
    def zip[S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11],m12: (=>T12) => Matcher[S12],m13: (=>T13) => Matcher[S13],m14: (=>T14) => Matcher[S14],m15: (=>T15) => Matcher[S15],m16: (=>T16) => Matcher[S16],m17: (=>T17) => Matcher[S17],m18: (=>T18) => Matcher[S18],m19: (=>T19) => Matcher[S19],m20: (=>T20) => Matcher[S20]): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20)] =
      outer.zip(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20)(t)
  }


  def zip[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21, S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20,S21](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11],m12: (=>T12) => Matcher[S12],m13: (=>T13) => Matcher[S13],m14: (=>T14) => Matcher[S14],m15: (=>T15) => Matcher[S15],m16: (=>T16) => Matcher[S16],m17: (=>T17) => Matcher[S17],m18: (=>T18) => Matcher[S18],m19: (=>T19) => Matcher[S19],m20: (=>T20) => Matcher[S20],m21: (=>T21) => Matcher[S21]):
  (=>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21)) => Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20,S21)] = {

    def zip1(expected: =>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21)) = ((actual: (S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20,S21)) => {
      val r = matchField(m1, expected._1, actual._1, "_1") and matchField(m2, expected._2, actual._2, "_2") and matchField(m3, expected._3, actual._3, "_3") and matchField(m4, expected._4, actual._4, "_4") and matchField(m5, expected._5, actual._5, "_5") and matchField(m6, expected._6, actual._6, "_6") and matchField(m7, expected._7, actual._7, "_7") and matchField(m8, expected._8, actual._8, "_8") and matchField(m9, expected._9, actual._9, "_9") and matchField(m10, expected._10, actual._10, "_10") and matchField(m11, expected._11, actual._11, "_11") and matchField(m12, expected._12, actual._12, "_12") and matchField(m13, expected._13, actual._13, "_13") and matchField(m14, expected._14, actual._14, "_14") and matchField(m15, expected._15, actual._15, "_15") and matchField(m16, expected._16, actual._16, "_16") and matchField(m17, expected._17, actual._17, "_17") and matchField(m18, expected._18, actual._18, "_18") and matchField(m19, expected._19, actual._19, "_19") and matchField(m20, expected._20, actual._20, "_20") and matchField(m21, expected._21, actual._21, "_21")
      (r.isSuccess, "For "+expected+"\n"+r.message)
    }): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20,S21)]
    zip1
  }

  implicit class TupleMatcher21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21)) {
    def zip[S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20,S21](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11],m12: (=>T12) => Matcher[S12],m13: (=>T13) => Matcher[S13],m14: (=>T14) => Matcher[S14],m15: (=>T15) => Matcher[S15],m16: (=>T16) => Matcher[S16],m17: (=>T17) => Matcher[S17],m18: (=>T18) => Matcher[S18],m19: (=>T19) => Matcher[S19],m20: (=>T20) => Matcher[S20],m21: (=>T21) => Matcher[S21]): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20,S21)] =
      outer.zip(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21)(t)
  }


  def zip[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22, S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20,S21,S22](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11],m12: (=>T12) => Matcher[S12],m13: (=>T13) => Matcher[S13],m14: (=>T14) => Matcher[S14],m15: (=>T15) => Matcher[S15],m16: (=>T16) => Matcher[S16],m17: (=>T17) => Matcher[S17],m18: (=>T18) => Matcher[S18],m19: (=>T19) => Matcher[S19],m20: (=>T20) => Matcher[S20],m21: (=>T21) => Matcher[S21],m22: (=>T22) => Matcher[S22]):
  (=>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22)) => Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20,S21,S22)] = {

    def zip1(expected: =>(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22)) = ((actual: (S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20,S21,S22)) => {
      val r = matchField(m1, expected._1, actual._1, "_1") and matchField(m2, expected._2, actual._2, "_2") and matchField(m3, expected._3, actual._3, "_3") and matchField(m4, expected._4, actual._4, "_4") and matchField(m5, expected._5, actual._5, "_5") and matchField(m6, expected._6, actual._6, "_6") and matchField(m7, expected._7, actual._7, "_7") and matchField(m8, expected._8, actual._8, "_8") and matchField(m9, expected._9, actual._9, "_9") and matchField(m10, expected._10, actual._10, "_10") and matchField(m11, expected._11, actual._11, "_11") and matchField(m12, expected._12, actual._12, "_12") and matchField(m13, expected._13, actual._13, "_13") and matchField(m14, expected._14, actual._14, "_14") and matchField(m15, expected._15, actual._15, "_15") and matchField(m16, expected._16, actual._16, "_16") and matchField(m17, expected._17, actual._17, "_17") and matchField(m18, expected._18, actual._18, "_18") and matchField(m19, expected._19, actual._19, "_19") and matchField(m20, expected._20, actual._20, "_20") and matchField(m21, expected._21, actual._21, "_21") and matchField(m22, expected._22, actual._22, "_22")
      (r.isSuccess, "For "+expected+"\n"+r.message)
    }): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20,S21,S22)]
    zip1
  }

  implicit class TupleMatcher22[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22)) {
    def zip[S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20,S21,S22](m1: (=>T1) => Matcher[S1],m2: (=>T2) => Matcher[S2],m3: (=>T3) => Matcher[S3],m4: (=>T4) => Matcher[S4],m5: (=>T5) => Matcher[S5],m6: (=>T6) => Matcher[S6],m7: (=>T7) => Matcher[S7],m8: (=>T8) => Matcher[S8],m9: (=>T9) => Matcher[S9],m10: (=>T10) => Matcher[S10],m11: (=>T11) => Matcher[S11],m12: (=>T12) => Matcher[S12],m13: (=>T13) => Matcher[S13],m14: (=>T14) => Matcher[S14],m15: (=>T15) => Matcher[S15],m16: (=>T16) => Matcher[S16],m17: (=>T17) => Matcher[S17],m18: (=>T18) => Matcher[S18],m19: (=>T19) => Matcher[S19],m20: (=>T20) => Matcher[S20],m21: (=>T21) => Matcher[S21],m22: (=>T22) => Matcher[S22]): Matcher[(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20,S21,S22)] =
      outer.zip(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22)(t)
  }

  private def matchField[T, S](m: (=>T) => Matcher[S], expected: T, actual: S, fieldName: String) =
    m(expected)(createExpectable(actual).updateDescription(s"  field $fieldName: " + _))
}


/**
 * Generation code for the MatcherZipOperators trait
 */
trait MatcherZipOperatorsCodeGeneration { outer =>
  def genZipOperators = (2 to 22).map(genZipOperator).mkString("\n")
  def genZipOperator(n: Int) = {
    val Ts = (1 to n).map("T"+_).mkString(",")
    val Ss = (1 to n).map("S"+_).mkString(",")
    val nl = "\\n"

    val matcherFunctions = (1 to n).map(i => s"m$i: (=>T$i) => Matcher[S$i]").mkString(",")
    val matchAllFields =
      (1 to n).map(i => s"""matchField(m$i, expected._$i, actual._$i, "_$i")""").mkString(" and ")

    s"""def zip[$Ts, $Ss]($matcherFunctions): (=>($Ts)) => Matcher[($Ss)] = {
        def zip1(expected: =>($Ts)) = ((actual: ($Ss)) => {
          val r = $matchAllFields
          (r.isSuccess, "For "+expected+"$nl"+r.message)
        }): Matcher[($Ss)]
        zip1
      }

      implicit class TupleMatcher$n[$Ts](t: ($Ts)) {
        def zip[$Ss]($matcherFunctions): Matcher[($Ss)] =
          outer.zip(${(1 to n).map("m"+_).mkString(",")})(t)
      }"""
  }
}

object MatcherZipOperators extends MatcherZipOperators

/**
 * This trait can be mixed in to remove the implicit definitions for zipping matchers
 */
trait NoMatcherZipOperatorsImplicits extends MatcherZipOperators {
  override def ContainSeqMatcherFunction[T](seq: Seq[T]) = super.ContainSeqMatcherFunction(seq)

  override def TupleMatcher2[T1,T2](t: (T1,T2)) = super.TupleMatcher2(t)
  override def TupleMatcher3[T1,T2,T3](t: (T1,T2,T3)) = super.TupleMatcher3(t)
  override def TupleMatcher4[T1,T2,T3,T4](t: (T1,T2,T3,T4)) = super.TupleMatcher4(t)
  override def TupleMatcher5[T1,T2,T3,T4,T5](t: (T1,T2,T3,T4,T5)) = super.TupleMatcher5(t)
  override def TupleMatcher6[T1,T2,T3,T4,T5,T6](t: (T1,T2,T3,T4,T5,T6)) = super.TupleMatcher6(t)
  override def TupleMatcher7[T1,T2,T3,T4,T5,T6,T7](t: (T1,T2,T3,T4,T5,T6,T7)) = super.TupleMatcher7(t)
  override def TupleMatcher8[T1,T2,T3,T4,T5,T6,T7,T8](t: (T1,T2,T3,T4,T5,T6,T7,T8)) = super.TupleMatcher8(t)
  override def TupleMatcher9[T1,T2,T3,T4,T5,T6,T7,T8,T9](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9)) = super.TupleMatcher9(t)
  override def TupleMatcher10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10)) = super.TupleMatcher10(t)
  override def TupleMatcher11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11)) = super.TupleMatcher11(t)
  override def TupleMatcher12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12)) = super.TupleMatcher12(t)
  override def TupleMatcher13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13)) = super.TupleMatcher13(t)
  override def TupleMatcher14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14)) = super.TupleMatcher14(t)
  override def TupleMatcher15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15)) = super.TupleMatcher15(t)
  override def TupleMatcher16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16)) = super.TupleMatcher16(t)
  override def TupleMatcher17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17)) = super.TupleMatcher17(t)
  override def TupleMatcher18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18)) = super.TupleMatcher18(t)
  override def TupleMatcher19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19)) = super.TupleMatcher19(t)
  override def TupleMatcher20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20)) = super.TupleMatcher20(t)
  override def TupleMatcher21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21)) = super.TupleMatcher21(t)
  override def TupleMatcher22[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22](t: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22)) = super.TupleMatcher22(t)
}

/**
 * code generation for the NoMatcherZipOperatorsImplicits trait
 */
trait NoMatcherZipOperatorsImplicitsCodeGeneration {
  lazy val gen22ZipOperatorsNoImplicits = (2 to 22).map(genZipOperatorNoImplicit).mkString("\n")

  def genZipOperatorNoImplicit(n: Int) = {
    val Ts = (1 to n).map("T"+_).mkString(",")
    s"""override def TupleMatcher$n[$Ts](t: ($Ts)) = super.TupleMatcher$n(t)"""
  }
}

