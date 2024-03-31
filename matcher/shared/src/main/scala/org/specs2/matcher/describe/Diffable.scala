package org.specs2
package matcher.describe

import scala.util.{Failure, Try}
import PrimitiveDiffable.primitive
import scala.deriving.*
import scala.compiletime.{erasedValue, error, constValue, summonInline}

/** Typeclass for values which can be compared and return a comparison result
  */
trait Diffable[T]:
  def diff(actual: T, expected: T): ComparisonResult

  def unsafeDiff(actual: Any, expected: Any): ComparisonResult =
    diff(actual.asInstanceOf[T], expected.asInstanceOf[T])

/** Note: variance is not managed by having a Diffable typeclass with a contravariant parameter Diffable[-T] In that the
  * implicit for case classes (see `product`) can not be found.
  *
  * Instead we deal with variance at the level of each implicit.
  *
  * For example `given exceptionDiffable[T <: Throwable]: Diffable[T] = new ThrowableDiffable[T]` will be found for any
  * custom exception extending Throwable. Similarly we can get a diff for Right[String, Int](1) and Right[String,
  * Int](2) with `given eitherDiffable[L: Diffable, R: Diffable, T <: Either[L, R]]` because we unify both Rights with
  * Either[String, Int]
  */
object Diffable extends DiffableLowImplicits:

  def diff[T](actual: T, expected: T)(using di: Diffable[T]): ComparisonResult =
    di.diff(actual, expected)

  // scala collections
  given mapDiffable[K: Diffable, V: Diffable, M <: Map[K, V]]: Diffable[M] = new MapDiffable[K, V, M]
  given seqDiffable[E: Diffable, S <: Seq[E]]: Diffable[S] = new SeqLinesDiffable[E, S]
  given setDiffable[E: Diffable, S <: Set[E]]: Diffable[S] = new SetDiffable[E, S]
  given arrayDiffable[E: Diffable]: Diffable[Array[E]] = new ArrayDiffable

  // Needed to avoid ambiguous implicits with Dotty when looking for a Diffable
  // for `Either[Int, Nothing]` for example.
  given nothingDiffable: Diffable[Nothing] = NothingDiffable

  // instances for primitive types
  given intDiffable: Diffable[Int] = primitive
  given booleanDiffable: Diffable[Boolean] = primitive
  given stringDiffable: Diffable[String] = primitive
  given longDiffable: Diffable[Long] = primitive
  given floatDiffable: Diffable[Float] = primitive
  given doubleDiffable: Diffable[Double] = primitive

  // basic elements
  given stackTraceElementDiffable: Diffable[StackTraceElement] = new StackTraceElementDiffable
  given exceptionDiffable[T <: Throwable]: Diffable[T] = new ThrowableDiffable[T]

  // None type
  given optionNoneDiffable[T <: Option[Nothing]]: Diffable[T] = new OptionNoneDiffable[T]

  given eitherRightDiffable[R: Diffable]: Diffable[Right[Nothing, R]] = new EitherRightDiffable[R]
  given eitherLeftDiffable[L: Diffable]: Diffable[Left[L, Nothing]] = new EitherLeftDiffable[L]

  given tryDiffable[T: Diffable, S <: Try[T]]: Diffable[S] = new TryDiffable[T, S]
  given failureDiffable: Diffable[Failure[Nothing]] = new FailureDiffable

trait DiffableLowImplicits extends DiffableLowImplicits2:
  given optionDiffable[T: Diffable, S <: Option[T]]: Diffable[S] = new OptionDiffable[T, S]
  given eitherDiffable[L: Diffable, R: Diffable, T <: Either[L, R]]: Diffable[T] = new EitherDiffable[L, R, T]

trait DiffableLowImplicits2 extends DiffableLowImplicits3:

  /** this Diff instance addresses case classes differences */
  inline given product[T](using m: Mirror.ProductOf[T]): Diffable[T] =
    derived[T]

  inline def derived[T](using p: Mirror.ProductOf[T]): Diffable[T] =
    lazy val diffables = summonAll[p.MirroredElemTypes]
    lazy val typeName: String = constValue[p.MirroredLabel]
    new ProductDiffable[T](typeName, p, diffables)

  inline def summonAll[T <: Tuple]: List[Diffable[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      // in the case of a large number of fields we summon the diffables 5 by 5 in order
      // to avoid hitting the max inlining limit of 32 by default
      case _: (t1 *: t2 *: t3 *: t4 *: t5 *: ts) =>
        summonInline[Diffable[t1]] :: summonInline[Diffable[t2]] :: summonInline[Diffable[t3]] ::
          summonInline[Diffable[t4]] :: summonInline[Diffable[t5]] :: summonAll[ts]
      case _: (t *: ts) => summonInline[Diffable[t]] :: summonAll[ts]

trait DiffableLowImplicits3:

  given fallbackDiffable[T]: Diffable[T] =
    new FallbackDiffable[T]

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
