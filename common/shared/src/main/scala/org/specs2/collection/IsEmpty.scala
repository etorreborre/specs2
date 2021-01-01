package org
package specs2
package collection

trait IsEmpty[T]:
  def isEmpty(t: T): Boolean

object IsEmpty extends IsEmptyLowPriority1:

  def apply[T](using ev: IsEmpty[T]): IsEmpty[T] =
    ev

  extension [T : IsEmpty](t: T)
    def isEmpty: Boolean =
      IsEmpty[T].isEmpty(t)

  given seqIsEmpty[T]: IsEmpty[Seq[T]] =
    new IsEmpty[Seq[T]]:
      def isEmpty(t: Seq[T]): Boolean =
        t.isEmpty

  given arrayIsEmpty[T]: IsEmpty[Array[T]] =
    new IsEmpty[Array[T]]:
      def isEmpty(t: Array[T]): Boolean =
        t.isEmpty

trait IsEmptyLowPriority1 extends IsEmptyLowPriority2:

  given listIsEmpty[T]: IsEmpty[List[T]] =
    new IsEmpty[List[T]]:
      def isEmpty(t: List[T]): Boolean =
        t.isEmpty

trait IsEmptyLowPriority2:

  given stringIsEmpty: IsEmpty[String] =
    new IsEmpty[String]:
      def isEmpty(t: String): Boolean =
        t.isEmpty
