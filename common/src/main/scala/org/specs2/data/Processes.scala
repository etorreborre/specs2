package org.specs2
package data

import java.util.concurrent.ExecutorService

import scalaz.stream.Process._
import scalaz.stream.{io, Process}
import scalaz.\/._
import scalaz.concurrent.{Future, Task}
import Task._
import scalaz.syntax.bind._
import scalaz.{Nondeterminism, Monoid}

/**
 * Useful functions for processes
 */
trait Processes {

  /** @return a process where each element is paired with the previous */
  def withPrevious[T]: Process1[T, (Option[T], T)] = {
    def go(previous: Option[T]): Process1[T, (Option[T], T)] =
      await1[T].flatMap { i =>
        emit((previous, i)) fby go(Some(i))
      }

    go(None)
  }

  /** @return a process where each element is paired with the next */
  def withNext[T]: Process1[T, (T, Option[T])] = {
    def go(previous: Option[T]): Process1[T, (T, Option[T])] =
      await1[T].flatMap { i =>
        previous.map(p => emit((p, Some(i))) fby go(Some(i))).getOrElse(go(Some(i))).orElse(emit((i, None)))
      }
    go(None)
  }

  /** @return a process where each element is paired with the previous and next */
  def withPreviousAndNext[T]: Process1[T, (Option[T], T, Option[T])] = {
    def go(previous: Option[T], current: Option[T]): Process1[T, (Option[T], T, Option[T])] =
      await1[T].flatMap { i =>
        current.map { c =>
          emit((previous, c, Some(i))) fby go(Some(c), Some(i))
        }.getOrElse(go(None, Some(i))).orElse(emit((current, i, None)))
      }
    go(None, None)
  }

  /**
   * Flatten a Process[Task, Seq[T]] into Process[Task, T]
   */
  implicit class ProcessSeqSyntax[T](ps: Process[Task, Seq[T]]) {
    def flatten: Process[Task, T] =
      ps.flatMap(ts => Process.emitAll(ts).toSource)
  }

  /**
   * additional operations for processes
   */
  implicit class processOps[T](ps: Process[Task, T]) {
    def andFinally(t: Task[Unit]): Process[Task, T] = {
      val sink: Sink[Task, T] =
        io.resource(Task.now(()))(u => t)(
          u => Task.now(u => Task.now(u)))

      ps.observe(sink)
    }
  }

  /** start an execution right away */
  def start[A](a: =>A)(executorService: ExecutorService) =
    new Task(Future(Task.Try(a))(executorService).start)

  /** syntax sugar for Processes */
  implicit class asLogged[F[_], A](process: Process[F, A]) {
    def logged: Writer[F, A, A] = Process.logged(process)
    def W: Writer[F, A, Nothing] = process.map(left)
  }

  implicit def functiontoW[F[_], T, A](process: T => Process[F, A]): T => Writer[F, A, Nothing] =
    (t: T) => process(t).W
}

object Processes extends Processes
