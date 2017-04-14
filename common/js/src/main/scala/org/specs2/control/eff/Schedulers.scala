package org.specs2.control.eff

import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js.timers._

object SchedulersJs {

  /**
   * Default Scheduler for JavaScript
   */
  def default: Scheduler = new Scheduler {
    def schedule(timedout: =>Unit, duration: FiniteDuration): () => Unit = {
      val handle = setTimeout(duration)(timedout)
      () => clearTimeout(handle)
    }

    def shutdown(): Unit = ()

    override def toString = "Scheduler"
  }

}
