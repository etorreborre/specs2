package org.specs2

import scala.concurrent.ExecutionContextExecutor

package object concurrent {

  private[concurrent] def parasitic =
    new ExecutionContextExecutor {
      override def execute(runnable: Runnable): Unit = runnable.run()

      override def reportFailure(cause: Throwable): Unit = cause.printStackTrace()
    }

}
