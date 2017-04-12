package org.specs2
package control

import org.specs2.fp._
import scalaz.concurrent._

object FuturezInstances {

  /** Applicative instance running futures in parallel for Scalaz */
  implicit val parallelApplicative = new Applicative[Future] {
    def point[A](a: => A): Future[A] = Future.futureInstance.point(a)
    def ap[A,B](fa: => Future[A])(f: => Future[A => B]): Future[B] =
      Future.futureInstance.mapBoth(fa, f)((a, function) => function(a))
  }

}
