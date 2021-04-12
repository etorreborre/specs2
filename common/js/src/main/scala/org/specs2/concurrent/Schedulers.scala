package org.specs2.concurrent

import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js.timers.*

trait Schedulers {

  /**
   * Default Scheduler for JavaScript
   */
  def default: Scheduler = new Scheduler {
    def schedule(action: =>Unit, duration: FiniteDuration): () => Unit = {
      val handle = setTimeout(duration)(action)
      () => clearTimeout(handle)
    }

    def shutdown() = ()

    override def toString = "Scheduler"
  }

}

object Schedulers extends Schedulers
