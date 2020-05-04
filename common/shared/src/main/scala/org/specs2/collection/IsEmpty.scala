package org
package specs2
package collection

trait IsEmpty[T] {
  def isEmpty(t: T): Boolean
}

object IsEmpty {

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
