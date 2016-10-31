package org.specs2
package matcher.describe

import org.specs2.matcher.describe.constraints.IsCaseClass
import org.specs2.matcher.describe.inspectable.PrimitiveDifferenceInspectable.primitive
import org.specs2.matcher.describe.inspectable._
import org.specs2.matcher.describe.introspection.{CaseClassDiffable, CaseClassIntrospection}
import shapeless.{HList, LabelledGeneric}

import scala.util.{Failure, Try}

abstract class Diffable[-T]{
  def diff( actual: T, expected: T ): ComparisonResult
}

object Diffable extends DiffableIntrospectors {

  // instances for primitive types
  implicit val intDiffable: Diffable[Int] = primitive
  implicit val booleanDiffable: Diffable[Boolean] = primitive
  implicit val stringDiffable: Diffable[String] = primitive
  implicit val longDiffable: Diffable[Long] = primitive
  implicit val floatDiffable: Diffable[Float] = primitive
  implicit val doubleDiffable: Diffable[Double] = primitive

  // basic elements
  implicit val stackTraceElementDiffable: Diffable[StackTraceElement] = new StackTraceElementDiffable
  implicit val exceptionDiffable: Diffable[Throwable] = new ThrowableDiffable

  //scala objects
  implicit def optionDiffable[T : Diffable]: Diffable[Option[T]] = new OptionDiffable[T]
  implicit val optionNoneDiffable: Diffable[Option[Nothing]] = OptionNoneDiffable

  implicit def eitherRightDiffable[R : Diffable]: Diffable[Right[Nothing, R]] = new EitherRightDiffable[R]
  implicit def eitherLeftDiffable[L : Diffable]: Diffable[Left[L, Nothing]] = new EitherLeftDiffable[L]
  implicit def eitherDiffable[L : Diffable, R : Diffable] = new EitherDiffable[L, R]

  implicit def tryDiffable[T : Diffable]: Diffable[Try[T]] = new TryDiffable[T]
  implicit val failureDiffable: Diffable[Failure[Nothing]] = new FailureDiffable


  // scala collections
  implicit def mapDiffable[K : Diffable, V : Diffable]: Diffable[Map[K, V]] = new MapDiffable[K, V]
  implicit def setDiffable[E: Diffable]: Diffable[Set[E]] = new SetDiffable
  implicit def seqDiffable[E: Diffable]: Diffable[Seq[E]] = new SeqDiffable
  implicit def arrayDiffable[E: Diffable]: Diffable[Array[E]] = new ArrayDiffable
}

trait DiffableIntrospectors extends Fallback {
  implicit def caseClassDiffable[T <: Product with Serializable: IsCaseClass, L <: HList](implicit labelled: LabelledGeneric.Aux[T, L],
                                                                                                    di: CaseClassIntrospection[L]): Diffable[T] = new CaseClassDiffable[T, L]
}

trait Fallback {
  implicit def fallbackDiffable[T]: Diffable[T] = new FallbackDiffable[T]
}


