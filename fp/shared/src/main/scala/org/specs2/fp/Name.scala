package org.specs2.fp

sealed abstract class Name[+A] {
  def value: A
}

sealed abstract class Need[+A] extends Name[A]

final case class Value[+A](value: A) extends Need[A]

object Name {
  def apply[A](a: => A) = new Name[A] {
    def value = a
  }

  def unapply[A](v: Name[A]): Option[A] = Some(v.value)

  implicit val name: Monad[Name] = new Monad[Name] {
    def point[A](a: => A): Name[A] = Name(a)

    def bind[A, B](fa: Name[A])(f: A => Name[B]): Name[B] =
      f(fa.value)
  }

}

object Need {
  def apply[A](a: => A): Name[A] = {
    new Need[A] {
      private[this] lazy val value0: A = a
      def value = value0
    }
  }
  def unapply[A](x: Need[A]): Option[A] = Some(x.value)
}
