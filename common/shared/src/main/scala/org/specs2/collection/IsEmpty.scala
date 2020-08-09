package org
package specs2
package collection

trait IsEmpty[T] {
  def isEmpty(t: T): Boolean
}

object IsEmpty extends IsEmptyLowPriority1 {

  def apply[T](implicit ev: IsEmpty[T]): IsEmpty[T] =
    ev

  implicit class IsEmptyOps[T : IsEmpty](t: T) {
    def isEmpty: Boolean =
      IsEmpty[T].isEmpty(t)
  }

  implicit def seqIsEmpty[T]: IsEmpty[Seq[T]] =
    new IsEmpty[Seq[T]] {
      def isEmpty(t: Seq[T]): Boolean =
        t.isEmpty
    }
}

trait IsEmptyLowPriority1 extends IsEmptyLowPriority2 {
  implicit def listIsEmpty[T]: IsEmpty[List[T]] =
    new IsEmpty[List[T]] {
      def isEmpty(t: List[T]): Boolean =
        t.isEmpty
    }
}

trait IsEmptyLowPriority2 {
  implicit def stringIsEmpty: IsEmpty[String] =
    new IsEmpty[String] {
      def isEmpty(t: String): Boolean =
        t.isEmpty
    }
}
