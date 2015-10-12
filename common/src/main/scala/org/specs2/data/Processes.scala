package org.specs2
package data

import java.util.concurrent.ExecutorService

import scalaz.stream.Cause.EarlyCause
import scalaz.stream._
import Process._
import scalaz.{Monad, Catchable, OptionT, Scalaz, \/}, Scalaz._, \/._
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
    def logged: Writer[F, A, A] = writer.logged(process)
    def W: Writer[F, A, Nothing] = process.map(left)
  }

  /**
   * additional operations for processes producing tasks
   */
  implicit class TaskProcessOps[T](ps: Process[Task, Task[T]]) {
    def sequence(nb: Int) =
      ps.pipe(process1.chunk(nb)).map(Nondeterminism[Task].gather).eval.flatMap(emitAll)
  }

  /**
   * Accumulate state on a Process[Task, T] using an accumulation action and
   * an initial state
   */
  def foldState1[S, T](action: (T, S) => S)(init: S): Process1[T, S] = {
    def go(state: S): Process1[T, S] =
      Process.receive1 { t: T =>
        val newState = action(t, state)
        emit(newState) fby go(newState)
      }

    go(init)
  }

  /**
   * Accumulate state on a Process[Task, T] using an accumulation action and
   * an initial state, but also keep the current element
   */
  def zipWithState1[S, T](action: (T, S) => S)(init: S): Process1[T, (T, S)] = {

    def go(state: S): Process1[T, (T, S)] =
      Process.receive1 { t: T =>
        val newState = action(t, state)
        emit((t, newState)) fby go(newState)
      }

    go(init)
  }

  /** create a Stepper for a given Process[F, A] */
  def step[A](p: Process[Task, A]): Stepper[Task, A] = new Stepper[Task, A] {
    var state = p

    def next: OptionT[Task, Seq[A]] = state.step match {

      case Halt(_) => OptionT.none

      case Step(Emit(as: Seq[A]), cont) =>
        state = cont.continue
        OptionT(as.point[Task] map some)

      // what should be done with the preempt parameter in scalaz-stream 0.8?
      case Step(Await(req: Task[_], rcv, preempt), cont) =>
        for {
          tail <- (req.attempt map { r => rcv(EarlyCause fromTaskResult r).run +: cont }).liftM[OptionT]
          _ = state = tail          // purity ftw!
          back <- next
        } yield back
    }

    def close =
      Task.suspend {
        Task.delay(state = state.kill) >>
        state.run
      }
  }

  /** start an execution right away */
  def start[A](a: =>A)(executorService: ExecutorService) =
    new Task(Future(Task.Try(a))(executorService).start)

  implicit def functiontoW[F[_], T, A](process: T => Process[F, A]): T => Writer[F, A, Nothing] =
    (t: T) => process(t).W
}

object Processes extends Processes

/** Helper trait to step through a Process[F, A] */
trait Stepper[F[_], A] {
  def next: OptionT[F, Seq[A]]
  def close: F[Unit]
}
