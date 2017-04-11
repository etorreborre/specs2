package org.specs2.concurrent

import scala.concurrent._
import org.specs2.fp._

trait FutureApplicative {

  implicit def ApplicativeFuture(implicit ec: ExecutionContext): Applicative[Future] = new Applicative[Future] {
    def point[A](a: =>A) = Future.successful(a)

    def ap[A, B](fa: =>Future[A])(ff: =>Future[A => B]): Future[B] =
      (fa, ff) match {
        case (fa1, ff1) =>
          Future.sequence(List(fa1, ff1)).map {
            case a :: f :: _ => f.asInstanceOf[A => B](a.asInstanceOf[A])
            case _ => sys.error("impossible")
          }
      }

    override def toString = "Applicative[Future]"
  }

}

object FutureApplicative extends FutureApplicative
