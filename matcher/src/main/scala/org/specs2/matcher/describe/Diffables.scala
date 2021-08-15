package org.specs2.matcher.describe

import scala.util.{Failure, Success, Try}
import org.specs2.fp.syntax.*
import scala.deriving.Mirror

/** Diffable instances for various types
  */

object PrimitiveDiffable:

  def primitive[T]: Diffable[T] = new Diffable[T] {
    def diff(left: T, right: T) =
      if left == right then PrimitiveIdentical(left)
      else PrimitiveDifference(left, right)
  }

object NothingDiffable extends Diffable[Nothing]:
  def diff(actual: Nothing, expected: Nothing): Nothing =
    throw new AssertionError(
      s"Critical error: attempting to compare values $actual and $expected of type Nothing, this type does not contain any value."
    )

class EitherDiffable[L: Diffable, R: Diffable, T <: Either[L, R]] extends Diffable[T]:

  def valdiff[A](a: A, e: A, isRight: Boolean)(using di: Diffable[A]) =
    val result = di.diff(a, e)
    if result.identical then EitherIdentical(result, isRight = isRight)
    else EitherDifferent(result, isRight = isRight)

  def diff(actual: T, expected: T) =
    (actual, expected) match
      case (Left(a), Left(e))   => valdiff(a, e, false)
      case (Right(a), Right(e)) => valdiff(a, e, true)
      case (a, e)               => EitherTypeDifferent(a.isRight)

class EitherRightDiffable[R](using rdi: Diffable[R]) extends Diffable[Right[Nothing, R]]:
  def diff(actual: Right[Nothing, R], expected: Right[Nothing, R]) =
    (actual, expected) match
      case (Right(a), Right(e)) =>
        val result = rdi.diff(a, e)
        if result.identical then EitherIdentical(result, isRight = true)
        else EitherDifferent(result, isRight = true)

class EitherLeftDiffable[L](using ldi: Diffable[L]) extends Diffable[Left[L, Nothing]]:
  def diff(actual: Left[L, Nothing], expected: Left[L, Nothing]) =
    (actual, expected) match
      case (Left(a), Left(e)) =>
        val result = ldi.diff(a, e)
        if result.identical then EitherIdentical(result, isRight = false)
        else EitherDifferent(result, isRight = true)

class OptionDiffable[T: Diffable, S <: Option[T]](using di: Diffable[T]) extends Diffable[S]:
  def diff(actual: S, expected: S): ComparisonResult =
    (actual, expected) match
      case (Some(a), Some(e)) =>
        val result = di.diff(a, e)
        if result.identical then OptionIdentical(Some(result))
        else OptionDifferent(result)
      case (None, None) => OptionIdentical(None)
      case _            => OptionTypeDifferent(actual.isDefined, expected.isDefined)

class OptionNoneDiffable[T <: Option[Nothing]] extends Diffable[T]:
  def diff(actual: T, expected: T) = OptionIdentical(None)

class TryDiffable[T: Diffable, S <: Try[T]](using tdi: Diffable[Throwable]) extends Diffable[S]:
  def valdiff[A](a: A, e: A, result: ComparisonResult, isSuccess: Boolean) =
    if result.identical then TryIdentical(a, isSuccess = isSuccess)
    else TryDifferent(result, isSuccess = isSuccess)

  def diff(actual: S, expected: S) =
    (actual, expected) match
      case (Failure(a), Failure(e)) => valdiff(a, e, tdi.diff(a, e), false)
      case (Success(a), Success(e)) => valdiff(a, e, summon[Diffable[T]].diff(a, e), true)
      case (a, e)                   => TryTypeDifferent(a.isSuccess)

class FailureDiffable(using di: Diffable[Throwable]) extends Diffable[Failure[Nothing]]:
  def diff(actual: Failure[Nothing], expected: Failure[Nothing]) =
    val (a, e) = (actual.exception, expected.exception)
    val result = di.diff(a, e)
    if result.identical then TryIdentical(a, isSuccess = false)
    else TryDifferent(result, isSuccess = false)

class MapDiffable[K, V, M <: Map[K, V]](using diff: Diffable[V]) extends Diffable[M]:

  def diff(actual: M, expected: M) =
    val changed = findChanged(actual, expected)
    val added = findAdded(actual, expected)
    val removed = findRemoved(actual, expected)
    val keys = (changed ++ added ++ removed).map(_._1)
    val identical = actual.view.filterKeys(k => !keys.contains(k)).toSeq
    if keys.isEmpty then MapIdentical(actual)
    else MapDifference(identical, changed, added, removed)

  private def findChanged(actual: Map[K, V], expected: Map[K, V]) =
    for
      k <- actual.keySet.intersect(expected.keySet).toSeq
      v1 <- actual.get(k)
      v2 <- expected.get(k)
      result = diff.diff(v1, v2)
      if !result.identical
    yield (k, result)

  private def findAdded(actual: Map[K, V], expected: Map[K, V]) =
    expected.view.filterKeys(k => !actual.contains(k)).toSeq

  private def findRemoved(actual: Map[K, V], expected: Map[K, V]) =
    actual.view.filterKeys(k => !expected.contains(k)).toSeq

class StackTraceElementDiffable(using nameDiffable: Diffable[String], lineDiffable: Diffable[Int])
    extends Diffable[StackTraceElement]:

  def diff(actual: StackTraceElement, expected: StackTraceElement) =
    if actual == expected then StackElementIdentical(actual)
    else
      StackElementDifferent(
        nameDiffable.diff(actual.getClassName, expected.getClassName),
        nameDiffable.diff(actual.getMethodName, expected.getMethodName),
        (Option(actual.getFileName) |@| Option(expected.getFileName))(nameDiffable.diff),
        lineDiffable.diff(actual.getLineNumber, expected.getLineNumber)
      )

class ThrowableDiffable[T <: Throwable](using sdi: Diffable[String], adi: Diffable[List[StackTraceElement]])
    extends Diffable[T]:

  def diff(actual: T, expected: T) =
    val messageResult = sdi.diff(actual.getMessage, expected.getMessage)
    if messageResult.identical then
      val stacktraceResult = adi.diff(actual.getStackTrace.toList, expected.getStackTrace.toList)
      if stacktraceResult.identical then ThrowableIdentical(actual)
      else ThrowableDifferentStackTrace(stacktraceResult)
    else ThrowableDifferentMessage(messageResult)

class SetDiffable[E, S <: Set[E]] extends Diffable[S]:

  def diff(actual: S, expected: S): ComparisonResult =
    if actual == expected then SetIdentical(actual)
    else
      SetDifference(
        same = findSame(actual, expected),
        added = findAdded(actual, expected),
        removed = findRemoved(actual, expected)
      )

  private def findSame(actual: Set[E], expected: Set[E]): Seq[E] =
    actual.intersect(expected).toSeq

  private def findAdded(actual: Set[E], expected: Set[E]): Seq[E] =
    expected.diff(actual).toSeq

  private def findRemoved(actual: Set[E], expected: Set[E]): Seq[E] =
    actual.diff(expected).toSeq

/** This diffable uses the Lines diffables to show differences between 2 sequences as a unified sequence with inlined
  * differences
  */
class SeqLinesDiffable[E, S <: Seq[E]](using di: Diffable[E]) extends Diffable[S]:

  def diff(actual: S, expected: S) =
    LinesDiffable.linesDiffable[E].diff(actual.toList, expected.toList)

/** This diffable displays elements missing or added from a Seq
  */
class SeqDiffable[E, S <: Seq[E]](using di: Diffable[E]) extends Diffable[S]:

  def diff(actual: S, expected: S) =
    val diffs = compareExisting(actual, expected)
    if actual.length == expected.length && diffs.forall(_.identical) then SeqIdentical(actual)
    else SeqDifference(result = diffs, added = expected.drop(actual.length), removed = actual.drop(expected.length))

  private def compareExisting(actual: Seq[E], expected: Seq[E]) =
    actual
      .zip(expected)
      .map { case (a, e) => di.diff(a, e) }

class ArrayDiffable[E](using di: Diffable[E]) extends Diffable[Array[E]]:

  def diff(actual: Array[E], expected: Array[E]) =
    val result = compareExisting(actual, expected)
    if actual.length == expected.length && result.toSeq.forall(_.identical) then ArrayIdentical(actual.toIndexedSeq)
    else
      ArrayDifference(
        results = result,
        added = expected.drop(actual.length).toIndexedSeq,
        removed = actual.drop(expected.length).toIndexedSeq
      )

  private def compareExisting(actual: Array[E], expected: Array[E]) =
    actual.zip(expected).toIndexedSeq.map { case (a, e) => di.diff(a, e) }

class FallbackDiffable[T] extends Diffable[T]:
  def diff(actual: T, expected: T) =
    (actual.asInstanceOf[Matchable], expected.asInstanceOf[Matchable]) match
      case (e1: Array[?], e2: Array[?]) =>
        Diffable.diff(e1.toIndexedSeq.map(a => a: Any), e2.toIndexedSeq.map(a => a: Any))
      case (a, e) if a == e =>
        OtherIdentical(actual, expected)
      case (a, e) =>
        OtherDifferent(actual, expected)

class ProductDiffable[T](typeName: String, product: Mirror.ProductOf[T], diffables: =>List[Diffable[?]]) extends Diffable[T]:
  def diff(actual: T, expected: T): ComparisonResult =
    val actualFields = actual.asInstanceOf[Product].productElementNames.toList.zip(actual.asInstanceOf[Product].productIterator.toList)
    val expectedValues = expected.asInstanceOf[Product].productIterator.toList
    val all = actualFields.zip(expectedValues).zip(diffables)
    val results: List[(String, ComparisonResult)] = all.map {
      case (((actualName, actualValue), expectedValue), d) =>
        (actualName, d.unsafeDiff(actualValue, expectedValue))
    }
    ProductComparisonResult(typeName, results)
