package org.specs2
package data

import java.io.PrintWriter

import io.FilePath
import foldm._, stream._, FoldProcessM._, FoldableProcessM._
import scalaz._, Scalaz._
import scalaz.concurrent.Task
import org.specs2.codata._

/**
 * Utility functions for folds
 */
object Fold {

  /** create a fold sink to output lines to a file */
  def showToFilePath[T : Show, S](path: FilePath): Sink[Task, T] =
    Processes.resource(Task.delay(new PrintWriter(path.path)))(p => Task.delay(p.close))(
      p => Task((t: T) => Task.delay(p.write(Show[T].shows(t)))))

  /**
   * Run a single fold
   */
  def runFold[T, S](process: Process[Task, T], fold: FoldTask[T, S]): Task[S] =
    fold.run[ProcessTask](process)

  /**
   * Run a list of SinkM, sequenced with the SinkM Monoid
   */
  def runSinks[T](process: ProcessTask[T], sinks: List[SinkTask[T]]): Task[Unit] =
    sinks.suml.run[ProcessTask](process)

}
