package org.specs2
package matcher.describe

import scala.util.{Failure, Try}
import PrimitiveDiffable.primitive

/**
 * Typeclass for values which can be compared and return a comparison result
 */
trait Diffable[-T]:

  def diff(actual: T, expected: T): ComparisonResult


object Diffable extends DiffableLowPriority1:

  def diff[T](actual: T, expected: T)(using di: Diffable[T]): ComparisonResult =
    di.diff(actual, expected)

trait DiffableLowPriority1 extends DiffableLowPriority2:
  // Needed to avoid ambiguous implicits with Dotty when looking for a Diffable
  // for `Either[Int, Nothing]` for example.
  given nothingDiffable: Diffable[Nothing] = NothingDiffable

  // instances for primitive types
  given intDiffable    : Diffable[Int]     = primitive
  given booleanDiffable: Diffable[Boolean] = primitive
  given stringDiffable : Diffable[String]  = primitive
  given longDiffable   : Diffable[Long]    = primitive
  given floatDiffable  : Diffable[Float]   = primitive
  given doubleDiffable : Diffable[Double]  = primitive

  // basic elements
  given stackTraceElementDiffable: Diffable[StackTraceElement] = new StackTraceElementDiffable
  given exceptionDiffable: Diffable[Throwable] = new ThrowableDiffable

  //scala objects
  given optionNoneDiffable: Diffable[Option[Nothing]] = OptionNoneDiffable

  given eitherRightDiffable[R : Diffable]: Diffable[Right[Nothing, R]] = new EitherRightDiffable[R]
  given eitherLeftDiffable[L : Diffable]: Diffable[Left[L, Nothing]] = new EitherLeftDiffable[L]

  given tryDiffable[T : Diffable]: Diffable[Try[T]] = new TryDiffable[T]
  given failureDiffable: Diffable[Failure[Nothing]] = new FailureDiffable


  // scala collections
  given mapDiffable[K : Diffable, V : Diffable]: Diffable[Map[K, V]] = new MapDiffable[K, V]
  given setDiffable[E : Diffable]: Diffable[Set[E]] = new SetDiffable
  given seqDiffable[E : Diffable]: Diffable[Seq[E]] = new SeqLinesDiffable[E]
  given arrayDiffable[E : Diffable]: Diffable[Array[E]] = new ArrayDiffable

trait DiffableLowPriority2:
  given optionDiffable[T : Diffable]: Diffable[Option[T]] = new OptionDiffable[T]
  given eitherDiffable[L : Diffable, R : Diffable]: Diffable[Either[L, R]] = new EitherDiffable[L, R]
  given fallbackDiffable[T]: Diffable[T] = new FallbackDiffable[T]

trait Diffables:
  extension [T](diffable: Diffable[T])

    def compareWith(compare: (T, T) => Boolean): Diffable[T] =
      new Diffable[T]:
        def diff(actual: T, expected: T): ComparisonResult = new ComparisonResult {
          def identical: Boolean = compare(actual, expected)
          def render: String = diffable.diff(actual, expected).render
          override def render(indent: String): String = diffable.diff(actual, expected).render(indent)
        }

object Diffables extends Diffables
