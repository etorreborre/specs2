package org.specs2
package control

import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicInteger

/**
 * This factory creates named threads which can be prefixed by "specs2" to get a better understanding of thread dumps
 *
 * Contributed by @jedws
 */
private[specs2]
case class NamedThreadFactory(namePrefix: String,
                              group: ThreadGroup = Thread.currentThread.getThreadGroup,
                              priority: Int = Thread.currentThread.getPriority) extends ThreadFactory {

  private[this] val threadNumber = new AtomicInteger(1)

  require(priority >= Thread.MIN_PRIORITY, "priority too low: " + priority)
  require(priority <= Thread.MAX_PRIORITY, "priority too high: " + priority)

  def newThread(r: Runnable) = {
    val t = new Thread(group, r, namePrefix+"-"+threadNumber.getAndIncrement, 0)
    t.setPriority(priority)
    t
  }
}

