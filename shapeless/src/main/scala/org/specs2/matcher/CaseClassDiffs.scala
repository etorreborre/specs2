package org.specs2.matcher

import org.specs2.matcher.describe._
import shapeless.{HList, LabelledGeneric}

trait CaseClassDiffs extends DiffableLowPriority1 with DiffableLowPriorityCaseClass {
  // the either diffable needs to be inferred before the case class diffable
  implicit def eitherDiffable1[L : Diffable, R : Diffable]: Diffable[Either[L, R]] = new EitherDiffable[L, R]

  // the option diffable needs to be inferred before the case class diffable
  implicit def optionDiffable1[T : Diffable]: Diffable[Option[T]] = new OptionDiffable[T]

}

trait DiffableLowPriorityCaseClass extends DiffableLowPriority2 {

  /** this diffable instance provides better failure messages for case classes */
  implicit def caseClassDiffable[T <: Product with Serializable with AnyRef: IsCaseClass, L <: HList](
    implicit labelled: LabelledGeneric.Aux[T, L],
             di:       CaseClassIntrospection[L]): Diffable[T] = new CaseClassDiffable[T, L]

  // cancel the inherited fallbackDiffable implicit here, it will be found in the Diffable object eventually
  override def fallbackDiffable[T]: Diffable[T] = new FallbackDiffable[T]

  // cancel the inherited eitherDiffable implicit here, it will be found in the CaseClassDiffs trait directly
  override def eitherDiffable[L : Diffable, R : Diffable]: Diffable[Either[L, R]] = new EitherDiffable[L, R]

  // cancel the inherited optionDiffable implicit here, it will be found in the CaseClassDiffs trait directly
  override def optionDiffable[T : Diffable]: Diffable[Option[T]] = new OptionDiffable[T]

}

object CaseClassDiffs extends CaseClassDiffs
