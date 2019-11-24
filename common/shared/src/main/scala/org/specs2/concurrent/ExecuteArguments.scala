package org.specs2.concurrent

object ExecuteArguments {
    /** default max threads number for the user execution environment */
    lazy val threadsNb: Int =
    math.max(Runtime.getRuntime.availableProcessors, 4)

  /** default threads number for the specs2 execution environment */
  lazy val specs2ThreadsNb: Int =
    math.max(Runtime.getRuntime.availableProcessors, 4)

}
