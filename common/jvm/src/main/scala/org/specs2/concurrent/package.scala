package org.specs2

import scala.concurrent.{ Await, Awaitable, Duration }

package object concurrent {

  private[specs2] def awaitResult[A](a: Awaitable[A], d: Duration) = {
    Await.result(a, d)
  }

}
