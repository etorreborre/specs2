package org.specs2.matcher.describe.inspectable

import org.specs2.matcher.describe._

import scala.util.{Failure, Success, Try}

object PrimitiveDifferenceInspectable {
  def primitive[T] = new Diffable[T] {
    def diff(left: T, right: T) =
      if (left == right) PrimitiveIdentical(left)
      else PrimitiveDifference(left, right)
  }
}

class EitherDiffable[L : Diffable, R : Diffable](implicit ldi: Diffable[L],
                                                 rdi: Diffable[R])
  extends Diffable[Either[L, R]] {
  def diff(actual: Either[L, R], expected: Either[L, R]) =
    (actual, expected) match {
      case (Left(a), Left(e)) if a == e => EitherIdentical(ldi.diff(a, e), isRight = false)
      case (Left(a), Left(e)) if a != e => EitherDifferent(ldi.diff(a, e), isRight = false)
      case (Right(a), Right(e)) if a == e => EitherIdentical(rdi.diff(a, e), isRight = true)
      case (Right(a), Right(e)) if a != e => EitherDifferent(rdi.diff(a, e), isRight = true)
      case (a, e) => EitherTypeDifferent(a.isRight)
    }
}

class EitherRightDiffable[R : Diffable](implicit rdi: Diffable[R])
  extends Diffable[Right[Nothing, R]] {
  def diff(actual: Right[Nothing, R], expected: Right[Nothing, R]) = (actual, expected) match {
    case (Right(a), Right(e)) if a == e => EitherIdentical(rdi.diff(a, e), isRight = true)
    case (Right(a), Right(e)) if a != e => EitherDifferent(rdi.diff(a, e), isRight = true)
  }
}

class EitherLeftDiffable[L : Diffable](implicit ldi: Diffable[L])
  extends Diffable[Left[L, Nothing]] {
  def diff(actual: Left[L, Nothing], expected: Left[L, Nothing]) =
    (actual, expected) match {
      case (Left(a), Left(e)) if a == e => EitherIdentical(ldi.diff(a, e), isRight = false)
      case (Left(a), Left(e)) if a != e => EitherDifferent(ldi.diff(a, e), isRight = false)
    }
}

class OptionDiffable[T : Diffable](implicit di: Diffable[T])
  extends Diffable[Option[T]] {

  def diff(actual: Option[T], expected: Option[T]): ComparisonResult =
    (actual, expected) match {
      case (Some(_), None) | (None, Some(_))  => OptionTypeDifferent(actual.isDefined, expected.isDefined)
      case (a, e) if a == e                   => OptionIdentical( compare(a, e) )
      case (Some(a), Some(e)) if a != e       => OptionDifferent(di.diff(a, e))
    }

  private def compare(actual: Option[T], expected: Option[T]): Option[ComparisonResult] =
    for {
      x <- actual
      y <- expected
    } yield di.diff(x, y)
}

case object OptionNoneDiffable extends Diffable[Option[Nothing]] {
  def diff(actual: Option[Nothing], expected: Option[Nothing]) = OptionIdentical(None)
}

class TryDiffable[T : Diffable](implicit di: Diffable[T],
                                tdi: Diffable[Throwable])
  extends Diffable[Try[T]] {

  def diff(actual: Try[T], expected: Try[T]) = (actual, expected) match {
    case (Success(a), Success(e)) if a == e => TryIdentical(a, isSuccess = true)
    case (Failure(a), Failure(e)) if a == e => TryIdentical(a, isSuccess = false)
    case (Success(a), Success(e)) => TryDifferent( di.diff(a, e), isSuccess = true)
    case (Failure(a), Failure(e)) => TryDifferent( tdi.diff(a, e), isSuccess = false)
    case (a, e) => TryTypeDifferent(a.isSuccess)
  }
}

class FailureDiffable(implicit di: Diffable[Throwable]) extends Diffable[Failure[Nothing]] {
  def diff(actual: Failure[Nothing], expected: Failure[Nothing]) = (actual, expected) match {
    case (Failure(a), Failure(e)) if a == e => TryIdentical(a, isSuccess = false)
    case (Failure(a), Failure(e)) => TryDifferent( di.diff(a, e), isSuccess = false)
  }
}


class MapDiffable[K, V](implicit diff: Diffable[V])
  extends Diffable[Map[K, V]] {

  def diff(actual: Map[K, V], expected: Map[K, V]) =
    if (actual == expected) MapIdentical(actual)
    else MapDifference(findIdentical(actual, expected),
                       findChanged(actual, expected),
                       findAdded(actual, expected),
                       findRemoved(actual, expected))


  private def findIdentical(actual: Map[K, V], expected: Map[K, V]) =
    actual.toSeq.intersect( expected.toSeq )

  private def findChanged(actual: Map[K, V], expected: Map[K, V]) =
    for {
      k <- actual.keySet.intersect(expected.keySet).toSeq
      v1 <- actual.get(k)
      v2 <- expected.get(k)
      if v1 != v2
    } yield (k, diff.diff(v1, v2))

  private def findAdded(actual: Map[K, V], expected: Map[K, V]) =
    expected.filterKeys( k => !actual.contains(k)).toSeq

  private def findRemoved(actual: Map[K, V], expected: Map[K, V]) =
    actual.filterKeys( k => !expected.contains(k)).toSeq
}

class StackTraceElementDiffable(implicit sdi: Diffable[String],
                                idi: Diffable[Int])
  extends Diffable[StackTraceElement] {

  def diff(actual: StackTraceElement, expected: StackTraceElement) =
    if (actual == expected) StackElementIdentical(actual)
    else StackElementDifferent(sdi.diff(actual.getClassName, expected.getClassName),
                               sdi.diff(actual.getMethodName, expected.getMethodName),
                               for {
                                 a <- Option(actual.getFileName)
                                 e <- Option(expected.getFileName)
                               } yield sdi.diff(a, e),
                               idi.diff(actual.getLineNumber, expected.getLineNumber) )
}

class ThrowableDiffable(implicit adi: Diffable[Array[StackTraceElement]])
  extends Diffable[Throwable] {
  // todo: compare cause, also model the response

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
    else SetDifference(identical = findIdentical(actual, expected),
                       added = findAdded(actual, expected),
                       removed = findRemoved(actual, expected))

  private def findIdentical(actual: Set[E], expected: Set[E]): Seq[E] =
    actual.intersect( expected ).toSeq

  private def findAdded(actual: Set[E], expected: Set[E]): Seq[E] =
    expected.diff( actual ).toSeq

  private def findRemoved(actual: Set[E], expected: Set[E]): Seq[E] =
    actual.diff( expected ).toSeq
}

class SeqDiffable[E](implicit di: Diffable[E]) extends Diffable[Seq[E]] {

  def diff(actual: Seq[E], expected: Seq[E]) =
    if (actual == expected) SeqIdentical(actual)
    else SeqDifference(result = compareExisting(actual, expected),
                       added = expected.drop( actual.length ),
                       removed = actual.drop( expected.length ) )

  private def compareExisting(actual: Seq[E], expected: Seq[E]) =
    actual.zip(expected)
          .map { case (a, e) => di.diff(a, e) }
}

class ArrayDiffable[E](implicit di: Diffable[E]) extends Diffable[Array[E]] {

  def diff(actual: Array[E], expected: Array[E]) =
    if (actual.deep == expected.deep) ArrayIdentical(actual)
    else ArrayDifference(result = compareExisting(actual, expected),
                         added = expected.drop( actual.length ),
                         removed = actual.drop( expected.length ) )

  private def compareExisting(actual: Array[E], expected: Array[E]) =
    actual.zip(expected)
          .map { case (a, e) => di.diff(a, e) }
}

class FallbackDiffable[T] extends Diffable[T] {
  def diff(actual: T, expected: T) =
    if (actual == expected) OtherIdentical(actual)
    else OtherDifferent(actual, expected)
}

