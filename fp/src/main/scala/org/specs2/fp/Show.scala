package org.specs2.fp

/**
 * Inspired from the scalaz (https://github.com/scalaz/scalaz) project
 */
trait Show[F] {

  def show(f: F): String

}

object Show {

  @inline def apply[F](implicit F: Show[F]): Show[F] = F

  def showFromToString[A]: Show[A] = new Show[A] {
    override def show(f: A): String = f.toString
  }

  def show[A](f: A => String): Show[A] = new Show[A] {
    override def show(a: A): String = f(a)
  }

  implicit def intShow: Show[Int] =
    showFromToString[Int]

}

trait ShowSyntax {

  implicit class ShowOps[A : Show](a: A) {
    def show: String =
      Show.apply[A].show(a)
  }

}
