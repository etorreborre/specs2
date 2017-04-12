package org.specs2.concurrent

import scala.concurrent.duration._

/**
 * The design of the Scheduler is taken from:
 * https://github.com/functional-streams-for-scala/fs2/blob/series/1.0/core/jvm/src/main/scala/fs2/Scheduler.scala
 */
trait Scheduler {

  def schedule(timedout: =>Unit, duration: FiniteDuration): () => Unit

  def shutdown(): Unit

}
