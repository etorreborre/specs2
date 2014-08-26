package org.specs2
package data

import java.util.concurrent.ExecutorService

import scalaz.stream._
import Process._
import scalaz.\/._
import scalaz.concurrent.{Future, Task}
import Task._
import scalaz.Nondeterminism

/**
 * Useful functions for processes
 */
trait Processes {

  /**
   * Flatten a Process[Task, Seq[T]] into Process[Task, T]
   */
  implicit class ProcessSeqOps[T](ps: Process[Task, Seq[T]]) {
    def flatten: Process[Task, T] =
      ps.flatMap(ts => Process.emitAll(ts).toSource)
  }

  /**
   * additional operations for Task processes
   */
  implicit class ProcessOps[T](ps: Process[Task, T]) {
    def andFinally(t: Task[Unit]): Process[Task, T] = {
      val sink: Sink[Task, T] =
        io.resource(Task.now(()))(u => t)(
          _ => Task.now(_ => Task.now(())))

      ps.observe(sink)
    }
  }

  /**
   * additional operations for generic processes
   */
  implicit class AsLogged[F[_], A](process: Process[F, A]) {
    def logged: Writer[F, A, A] = Process.logged(process)
    def W: Writer[F, A, Nothing] = process.map(left)
  }

  /**
   * additional operations for processes producing tasks
   */
  implicit class TaskProcessOps[T](ps: Process[Task, Task[T]]) {
    def sequence(nb: Int) =
      ps.pipe(process1.chunk(nb)).map(Nondeterminism[Task].gather).eval.flatMap(emitAll)
  }

  /** start an execution right away */
  def start[A](a: =>A)(executorService: ExecutorService) =
    new Task(Future(Task.Try(a))(executorService).start)

  implicit def functiontoW[F[_], T, A](process: T => Process[F, A]): T => Writer[F, A, Nothing] =
    (t: T) => process(t).W
}

object Processes extends Processes
