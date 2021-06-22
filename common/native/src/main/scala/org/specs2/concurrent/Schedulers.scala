package org.specs2.concurrent

import scala.concurrent.duration.FiniteDuration

trait Schedulers {

  /**
   * Default Scheduler for Scala Native
   */
  def default: Scheduler = new Scheduler {
    def schedule(timedout: =>Unit, duration: FiniteDuration): () => Unit =
      () => ()

    def shutdown() = ()

    override def toString = "Scheduler"
  }

}

object Schedulers extends Schedulers

