package org.specs2
package reflect

/**
  * This trait adds some syntactic sugar to create a sequence of classes from the declaration of their types
  */
trait ClassesOf {
	
  def classesOf[T1 : ClassManifest, T2 : ClassManifest] = Seq(implicitly[ClassManifest[T1]].erasure, implicitly[ClassManifest[T2]].erasure)
	def classesOf[T1 : ClassManifest, T2 : ClassManifest, T3 : ClassManifest] = Seq(implicitly[ClassManifest[T1]].erasure, implicitly[ClassManifest[T3]].erasure, implicitly[ClassManifest[T3]].erasure)
	def classesOf[T1 : ClassManifest, T2 : ClassManifest, T3 : ClassManifest, T4 : ClassManifest] = Seq(implicitly[ClassManifest[T1]].erasure, implicitly[ClassManifest[T2]].erasure, implicitly[ClassManifest[T3]].erasure, implicitly[ClassManifest[T4]].erasure)
	def classesOf[T1 : ClassManifest, T2 : ClassManifest, T3 : ClassManifest, T4 : ClassManifest, T5 : ClassManifest] = Seq(implicitly[ClassManifest[T1]].erasure, implicitly[ClassManifest[T2]].erasure, implicitly[ClassManifest[T3]].erasure, implicitly[ClassManifest[T4]].erasure, implicitly[ClassManifest[T5]].erasure)
	
}

