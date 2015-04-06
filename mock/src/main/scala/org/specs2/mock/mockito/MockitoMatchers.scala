package org.specs2
package mock
package mockito

import org.mockito.Matchers
import org.hamcrest.core.IsAnything
import scala.reflect.ClassTag

/**
 * Mockito Matchers for the most common types
 */
trait MockitoMatchers extends ArgThat {

  def anyString  = Matchers.anyString
  def anyBoolean = Matchers.anyBoolean
  def anyByte    = Matchers.anyByte
  def anyShort   = Matchers.anyShort
  def anyChar    = Matchers.anyChar
  def anyInt     = Matchers.anyInt
  def anyLong    = Matchers.anyLong
  def anyDouble  = Matchers.anyDouble
  def anyFloat   = Matchers.anyFloat

  def any[T : ClassTag]: T = org.mockito.Matchers.any(implicitly[ClassTag[T]].runtimeClass).asInstanceOf[T]

  def anyPartialFunction[T,R] = anArgThat(new IsAnything[PartialFunction[T,R]])

  def anyFunction1[T1,R] = anArgThat(new IsAnything[Function1[T1,R]])
  def anyFunction2[T1,T2,R] = anArgThat(new IsAnything[Function2[T1,T2,R]])
  def anyFunction3[T1,T2,T3,R] = anArgThat(new IsAnything[Function3[T1,T2,T3,R]])
  def anyFunction4[T1,T2,T3,T4,R] = anArgThat(new IsAnything[Function4[T1,T2,T3,T4,R]])
  def anyFunction5[T1,T2,T3,T4,T5,R] = anArgThat(new IsAnything[Function5[T1,T2,T3,T4,T5,R]])
  def anyFunction6[T1,T2,T3,T4,T5,T6,R] = anArgThat(new IsAnything[Function6[T1,T2,T3,T4,T5,T6,R]])
  def anyFunction7[T1,T2,T3,T4,T5,T6,T7,R] = anArgThat(new IsAnything[Function7[T1,T2,T3,T4,T5,T6,T7,R]])
  def anyFunction8[T1,T2,T3,T4,T5,T6,T7,T8,R] = anArgThat(new IsAnything[Function8[T1,T2,T3,T4,T5,T6,T7,T8,R]])
  def anyFunction9[T1,T2,T3,T4,T5,T6,T7,T8,T9,R] = anArgThat(new IsAnything[Function9[T1,T2,T3,T4,T5,T6,T7,T8,T9,R]])
  def anyFunction10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,R] = anArgThat(new IsAnything[Function10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,R]])
  def anyFunction11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,R] = anArgThat(new IsAnything[Function11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,R]])
  def anyFunction12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,R] = anArgThat(new IsAnything[Function12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,R]])
  def anyFunction13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,R] = anArgThat(new IsAnything[Function13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,R]])
  def anyFunction14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,R] = anArgThat(new IsAnything[Function14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,R]])
  def anyFunction15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,R] = anArgThat(new IsAnything[Function15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,R]])
  def anyFunction16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,R] = anArgThat(new IsAnything[Function16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,R]])
  def anyFunction17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,R] = anArgThat(new IsAnything[Function17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,R]])
  def anyFunction18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,R] = anArgThat(new IsAnything[Function18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,R]])
  def anyFunction19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,R] = anArgThat(new IsAnything[Function19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,R]])
  def anyFunction20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,R] = anArgThat(new IsAnything[Function20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,R]])
  def anyFunction21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,R] = anArgThat(new IsAnything[Function21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,R]])
  def anyFunction22[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22,R] = anArgThat(new IsAnything[Function22[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22,R]])

}

/**
 * GENERATION code
 */
//import reflect.Generation._
//
//object MockitoMatchersGeneration {
//  def main(args: Array[String]) {
//
//    FileWriter.writeFile("MockitoMatchers.scala",
//      (1 to 22).map { i =>
//          "def anyFunction"+i+typeParameters(i)+" = anArgThat(new IsAnything["+function(i)+"])"
//      }.mkString("\n", "\n", "\n"))
//  }
//
//}