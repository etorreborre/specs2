package org.specs2
package foldm
package stream

import FoldM._

import scalaz.stream._
import scalaz.stream.async.mutable.Signal
import scalaz.{Id, ~>}, Id._
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
    type S = Signal[T => Task[Unit]]
    def start = Task.now(async.toSignal(sink))
    def fold = (s: S, t: T) => { s.get.run.apply(t).run; s }
    def end(s: S) = s.close
  }

  def lift[T](f: T => Task[Unit]): SinkTask[T] =
    fromSink(Process.constant(f))
}
