package org.specs2
package control

import scala.concurrent.{ExecutionContext, Future}
import org.specs2.fp._

object FutureInstances {

  /** Applicative instance running futures in parallel for Scalaz */
  implicit def parallelApplicative(implicit ec: ExecutionContext) = new Applicative[Future] {
    def point[A](a: => A): Future[A] = Future(a)

    def ap[A,B](ffa: => Future[A])(ff: => Future[A => B]): Future[B] =
      Future.sequence(List(ffa, ff)).map {
        case a :: f :: _ => f.asInstanceOf[A => B](a.asInstanceOf[A])
        case _ => sys.error("impossible")
      }
  }

}

