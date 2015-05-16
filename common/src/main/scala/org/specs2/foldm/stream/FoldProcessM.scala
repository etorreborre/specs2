package org.specs2
package foldm
package stream

import FoldM._

import scalaz.stream.Process.{Halt, Await, Emit, Step}
import scalaz.stream._
import scalaz.{Id, ~>, \/, \/-, -\/}, Id._
import scalaz.concurrent.Task

object FoldProcessM {
  type ProcessTask[T] = Process[Task, T]
  type SinkTask[T] = SinkM[T, Task]
  type FoldTask[T, U] = FoldM[T, Task, U]

  implicit val IdTaskNaturalTransformation: Id ~> Task = new (Id ~> Task) {
    def apply[A](i: Id[A]): Task[A] = Task.now(i)
  }

  implicit val TaskProcessTaskNaturalTransformation: Task ~> ProcessTask = new (Task ~> ProcessTask) {
    def apply[A](t: Task[A]): Process[Task, A] = Process.eval(t)
  }

  implicit val IdProcessTaskNaturalTransformation: Id ~> ProcessTask = new (Id ~> ProcessTask) {
    def apply[A](t: Id[A]): Process[Task, A] = Process.eval(Task.now(t))
  }

  def fromSink[T](sink: Sink[Task, T]) = new FoldM[T, Task, Unit] {
    type S = Sink[Task, T] \/ (Sink[Task, T], T => Task[Unit])
    def start = Task.now(-\/(sink))

    def fold = (s: S, t: T) => {
      s match {
        case -\/(pr) =>
          pr.step match {
            case Step(Emit(emit), cont) =>
              val f = emit.head
              f(t).run
              \/-((cont.continue, f))

            case Step(Await(req, rcv), cont) =>
              val f: T => Task[Unit] = rcv(\/-(req.run)).run.take(1).runLastOr((t1: T) => Task.now(())).run
              f(t).run
              \/-((cont.continue, f))

            case h @ Halt(c) =>
              -\/(Process.halt)
          }
        case \/-((pr, f)) => f(t).run; s

      }
    }
    def end(s: S) = s.fold(_.kill.run, _._1.kill.run)
  }

  def lift[T](f: T => Task[Unit]): SinkTask[T] = new FoldM[T, Task, Unit] {
    type S = Unit
    def start = Task.now(())
    def fold = (s: S, t: T) => { f(t).run; s }
    def end(s: S) = Task.now(s)
  }
}
