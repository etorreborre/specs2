package org.specs2
package data

import io.FilePath
import foldm._, FoldM._, stream._, FoldProcessM._, FoldableProcessM._
import scalaz._, Scalaz._
import scalaz.concurrent.Task
import scalaz.stream._
import scalaz.stream.text._
import Process._
import Task._

/**
 * Utility functions for folds
 */
object Fold {

  /** create a fold sink to output lines to a file */
  def showToFilePath[T : Show, S](path: FilePath): Sink[Task, T] =
    io.fileChunkW(path.path).pipeIn(process1.lift(Show[T].shows) |> utf8Encode)

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


