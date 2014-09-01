package org.specs2
package matcher

import scalaz.concurrent.Task
import scalaz.stream.Process

/**
 * Matchers for Processes
 */
trait ProcessMatchers extends MustMatchers with MatchersImplicits {

  def haveLast[T](check: ValueCheck[T]): Matcher[Process[Task, T]] = { p: Process[Task, T] =>
    beSome(check).apply(createExpectable(p.runLog.run.lastOption))
  }

  def haveLog[T](checks: ValueCheck[T]*): Matcher[Process[Task, T]] = { p: Process[Task, T] =>
    contain(exactly(checks:_*)).apply(createExpectable(p.runLog.run))
  }
}

object ProcessMatchers extends ProcessMatchers