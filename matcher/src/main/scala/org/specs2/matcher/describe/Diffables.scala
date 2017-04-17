package org.specs2.matcher.describe

import scala.util.{Failure, Success, Try}
import scalaz._
import Scalaz._

/**
 * Diffable instances for various types
 */

object PrimitiveDiffable {

  def primitive[T]: Diffable[T] = new Diffable[T] {
    def diff(left: T, right: T) =
      if (left == right) PrimitiveIdentical(left)
      else PrimitiveDifference(left, right)
  }

}

class EitherDiffable[L : Diffable, R : Diffable]
  extends Diffable[Either[L, R]] {

  def valdiff[A](a: A, e: A, isRight: Boolean)(implicit di: Diffable[A]) = {
    val result = di.diff(a, e)
    if (result.identical) EitherIdentical(result, isRight = isRight)
    else EitherDifferent(result, isRight = isRight)
  }

  def diff(actual: Either[L, R], expected: Either[L, R]) =
    (actual, expected) match {
      case (Left(a), Left(e)) => valdiff(a, e, false)
      case (Right(a), Right(e)) => valdiff(a, e, true)
      case (a, e) => EitherTypeDifferent(a.isRight)
    }

}

class EitherRightDiffable[R](implicit rdi: Diffable[R]) extends Diffable[Right[Nothing, R]] {
  def diff(actual: Right[Nothing, R], expected: Right[Nothing, R]) =
    (actual, expected) match {
      case (Right(a), Right(e)) =>
        val result = rdi.diff(a, e)
        if (result.identical) EitherIdentical(result, isRight = true)
        else EitherDifferent(result, isRight = true)
    }
}

class EitherLeftDiffable[L](implicit ldi: Diffable[L]) extends Diffable[Left[L, Nothing]] {
  def diff(actual: Left[L, Nothing], expected: Left[L, Nothing]) =
    (actual, expected) match {
    case (Left(a), Left(e)) =>
      val result = ldi.diff(a, e)
      if (result.identical) EitherIdentical(result, isRight = false)
      else EitherDifferent(result, isRight = true)
    }
}

class OptionDiffable[T : Diffable](implicit di: Diffable[T]) extends Diffable[Option[T]] {
  def diff(actual: Option[T], expected: Option[T]): ComparisonResult =
    (actual, expected) match {
      case (Some(a), Some(e)) =>
        val result = di.diff(a, e)
        if (result.identical) OptionIdentical(Some(result))
        else OptionDifferent(result)
      case _ => OptionTypeDifferent(actual.isDefined, expected.isDefined)
    }
}

case object OptionNoneDiffable extends Diffable[Option[Nothing]] {
  def diff(actual: Option[Nothing], expected: Option[Nothing]) = OptionIdentical(None)
}

class TryDiffable[T : Diffable](implicit tdi: Diffable[Throwable]) extends Diffable[Try[T]] {
  def valdiff[A](a: A, e: A, isSuccess: Boolean)(implicit di: Diffable[A]) = {
    val result = di.diff(a, e)
    if (result.identical) TryIdentical(a, isSuccess = isSuccess)
    else TryDifferent(result, isSuccess = isSuccess)
  }

  def diff(actual: Try[T], expected: Try[T]) =
    (actual, expected) match {
      case (Failure(a), Failure(e)) => valdiff(a, e, false)
      case (Success(a), Success(e)) => valdiff(a, e, true)
      case (a, e) => TryTypeDifferent(a.isSuccess)
    }
}

class FailureDiffable(implicit di: Diffable[Throwable]) extends Diffable[Failure[Nothing]] {
  def diff(actual: Failure[Nothing], expected: Failure[Nothing]) = {
    val (a, e) = (actual.exception, expected.exception)
    val result = di.diff(a, e)
    if (result.identical) TryIdentical(a, isSuccess = false)
    else TryDifferent(result, isSuccess = false)
  }
}


class MapDiffable[K, V](implicit diff: Diffable[V]) extends Diffable[Map[K, V]] {

  def diff(actual: Map[K, V], expected: Map[K, V]) = {
    val changed = findChanged(actual, expected)
    val added = findAdded(actual, expected)
    val removed = findRemoved(actual, expected)
    val keys = (changed ++ added ++ removed).map(_._1)
    val identical = actual.filterKeys(k => !keys.contains(k)).toSeq
    if (keys.isEmpty) MapIdentical(actual)
    else MapDifference(identical, changed, added, removed)
  }

  private def findChanged(actual: Map[K, V], expected: Map[K, V]) =
    for {
      k <- actual.keySet.intersect(expected.keySet).toSeq
      v1 <- actual.get(k)
      v2 <- expected.get(k)
      result = diff.diff(v1, v2)
      if !result.identical
    } yield (k, result)

  private def findAdded(actual: Map[K, V], expected: Map[K, V]) =
    expected.filterKeys(k => !actual.contains(k)).toSeq

  private def findRemoved(actual: Map[K, V], expected: Map[K, V]) =
    actual.filterKeys(k => !expected.contains(k)).toSeq
}

class StackTraceElementDiffable(implicit nameDiffable: Diffable[String], lineDiffable: Diffable[Int]) extends Diffable[StackTraceElement] {

  def diff(actual: StackTraceElement, expected: StackTraceElement) =
    if (actual == expected)
      StackElementIdentical(actual)
    else
      StackElementDifferent(
        nameDiffable.diff(actual.getClassName, expected.getClassName),
        nameDiffable.diff(actual.getMethodName, expected.getMethodName),
        (Option(actual.getFileName) |@| Option(expected.getFileName))(nameDiffable.diff),
        lineDiffable.diff(actual.getLineNumber, expected.getLineNumber))
}

class ThrowableDiffable(implicit adi: Diffable[Array[StackTraceElement]]) extends Diffable[Throwable] {

  def diff(actual: Throwable, expected: Throwable) =
    adi.diff(actual.getStackTrace, expected.getStackTrace) match {
      case ArrayIdentical(_) => ThrowableIdentical(actual)
      case ArrayDifference(res, a, r) => ThrowableDifferent(res, a.asInstanceOf[Seq[StackTraceElement]], r.asInstanceOf[Seq[StackTraceElement]])
      case _ => throw new RuntimeException("should not happen")
    }
}

class SetDiffable[E] extends Diffable[Set[E]] {

  def diff(actual: Set[E], expected: Set[E]): ComparisonResult =
    if (actual == expected) SetIdentical(actual)
    else SetDifference(same = findSame(actual, expected),
      added = findAdded(actual, expected),
      removed = findRemoved(actual, expected))

  private def findSame(actual: Set[E], expected: Set[E]): Seq[E] =
    actual.intersect(expected).toSeq

  private def findAdded(actual: Set[E], expected: Set[E]): Seq[E] =
    expected.diff(actual).toSeq

  private def findRemoved(actual: Set[E], expected: Set[E]): Seq[E] =
    actual.diff(expected).toSeq
}

class SeqDiffable[E](implicit di: Diffable[E]) extends Diffable[Seq[E]] {

  def diff(actual: Seq[E], expected: Seq[E]) = {
    val diffs = compareExisting(actual, expected)
    if (actual.length == expected.length && diffs.forall(_.identical)) SeqIdentical(actual)
    else SeqDifference(result = diffs,
      added = expected.drop(actual.length),
      removed = actual.drop(expected.length))
  }

  private def compareExisting(actual: Seq[E], expected: Seq[E]) =
    actual.zip(expected)
      .map { case (a, e) => di.diff(a, e) }
}

class ArrayDiffable[E](implicit di: Diffable[E]) extends Diffable[Array[E]] {

  def diff(actual: Array[E], expected: Array[E]) = {
    val result = compareExisting(actual, expected)
    if (actual.length == expected.length && result.toSeq.forall(_.identical))
      ArrayIdentical(actual)
    else
      ArrayDifference(
        results = result,
        added   = expected.drop(actual.length),
        removed = actual.drop(expected.length))
  }

  private def compareExisting(actual: Array[E], expected: Array[E]) =
    actual.zip(expected).map { case (a, e) => di.diff(a, e) }
}

class FallbackDiffable[T] extends Diffable[T] {
  def diff(actual: T, expected: T) = {
    (actual, expected) match {
      case (e1: Array[_], e2: Array[_]) =>  Diffable.diff(e1.map(a => a:Any), e2.map(a => a:Any))
      case (a, e) if a == e =>              OtherIdentical(a)
      case (a, e) =>                        OtherDifferent(a, e)
    }
  }
}

