package org
package specs2
package collection

trait IsEmpty[T]:
  def isEmpty(t: T): Boolean

object IsEmpty extends IsEmptyLowPriority1:

  def apply[T](using ev: IsEmpty[T]): IsEmpty[T] =
    ev

  extension [T: IsEmpty](t: T)
    def isEmpty: Boolean =
      IsEmpty[T].isEmpty(t)

  given arrayIsEmpty[T]: IsEmpty[Array[T]] with
    def isEmpty(t: Array[T]): Boolean =
      t.isEmpty

  given seqIsEmpty[T]: IsEmpty[Seq[T]] with
    def isEmpty(t: Seq[T]): Boolean =
      t.isEmpty

trait IsEmptyLowPriority1 extends IsEmptyLowPriority2:

  given listIsEmpty[T]: IsEmpty[List[T]] with
    def isEmpty(t: List[T]): Boolean =
      t.isEmpty

  given optionIsEmpty[T]: IsEmpty[Option[T]] with
    def isEmpty(t: Option[T]): Boolean =
      !t.isDefined

  given eitherIsEmpty[E, T]: IsEmpty[Either[E, T]] with
    def isEmpty(t: Either[E, T]): Boolean =
      !t.toOption.isDefined

trait IsEmptyLowPriority2:

  given iterableOnceIsEmpty[T <: IterableOnce[?]]: IsEmpty[T] with
    def isEmpty(t: T): Boolean =
      t.iterator.isEmpty

  given stringIsEmpty: IsEmpty[String] with
    def isEmpty(t: String): Boolean =
      t.isEmpty
