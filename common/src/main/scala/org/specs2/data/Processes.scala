package org.specs2
package data

import java.util.concurrent.ExecutorService

import scalaz.stream._
import Process._
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

    // store the previous element and emit it once a new element occurs
    def go(previous: Option[T]): Process1[T, (T, Option[T])] = {
      val onEnd = previous.map(t => emit((t, None))).getOrElse(halt)
      receive1Or[T, (T, Option[T])](onEnd) { t: T =>
        previous.map(p => emit((p, Some(t))) fby go(Some(t))).getOrElse(go(Some(t)))
      }
    }
    go(None)
  }

  /** @return a process where each element is paired with the previous and next */
  def withPreviousAndNext[T]: Process1[T, (Option[T], T, Option[T])] = {
    def go(previous: Option[T], current: Option[T]): Process1[T, (Option[T], T, Option[T])] = {
      val onEnd = current.map(t => emit((previous, t, None))).getOrElse(halt)
      receive1Or[T, (Option[T], T, Option[T])](onEnd) { t: T =>
        current.map { c =>
          emit((previous, c, Some(t))) fby go(Some(c), Some(t))
        }.getOrElse(go(None, Some(t)))
      }
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

  /**
   * additional operations for processes producing tasks
   */
  implicit class taskProcessOps[T](ps: Process[Task, Task[T]]) {
    def sequence(nb: Int) =
      ps.pipe(process1.chunk(nb)).map(Nondeterminism[Task].gather).eval.flatMap(emitAll)
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
