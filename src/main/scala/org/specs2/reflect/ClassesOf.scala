package org.specs2
package reflect

import scala.reflect.ClassTag
import language.existentials

/**
  * This trait adds some syntactic sugar to create a sequence of classes from the declaration of their types
  */
trait ClassesOf {

  def classesOf[T1 : ClassTag, T2 : ClassTag] =
    Seq(implicitly[ClassTag[T1]].runtimeClass, implicitly[ClassTag[T2]].runtimeClass)

  def classesOf[T1 : ClassTag, T2 : ClassTag, T3 : ClassTag] =
    Seq(implicitly[ClassTag[T1]].runtimeClass, implicitly[ClassTag[T3]].runtimeClass, implicitly[ClassTag[T3]].runtimeClass)

  def classesOf[T1 : ClassTag, T2 : ClassTag, T3 : ClassTag, T4 : ClassTag] =
    Seq(implicitly[ClassTag[T1]].runtimeClass, implicitly[ClassTag[T2]].runtimeClass, implicitly[ClassTag[T3]].runtimeClass, implicitly[ClassTag[T4]].runtimeClass)

  def classesOf[T1 : ClassTag, T2 : ClassTag, T3 : ClassTag, T4 : ClassTag, T5 : ClassTag] =
    Seq(implicitly[ClassTag[T1]].runtimeClass,
        implicitly[ClassTag[T2]].runtimeClass,
        implicitly[ClassTag[T3]].runtimeClass,
        implicitly[ClassTag[T4]].runtimeClass,
        implicitly[ClassTag[T5]].runtimeClass)

}

