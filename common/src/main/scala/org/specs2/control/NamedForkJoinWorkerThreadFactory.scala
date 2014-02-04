package org.specs2
package control

import java.util.concurrent.atomic.AtomicInteger
import concurrent.forkjoin.ForkJoinPool.ForkJoinWorkerThreadFactory
import concurrent.forkjoin.ForkJoinPool

private[specs2]
case class NamedForkJoinWorkerThreadFactory(namePrefix: String,
                                            priority: Int = Thread.currentThread.getPriority) extends ForkJoinWorkerThreadFactory {

  private[this] val threadNumber = new AtomicInteger(1)

  require(priority >= Thread.MIN_PRIORITY, "priority too low: " + priority)
  require(priority <= Thread.MAX_PRIORITY, "priority too high: " + priority)

  private lazy val delegate = ForkJoinPool.defaultForkJoinWorkerThreadFactory

  def newThread(pool: ForkJoinPool) = {
    val t = delegate.newThread(pool)
    t.setName(namePrefix+"-"+threadNumber.getAndIncrement)
    t.setPriority(priority)
    t
  }
}

object NamedForkJoinWorkerThreadFactory {
  val specs2 = NamedForkJoinWorkerThreadFactory("specs2")
}

