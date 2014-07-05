package org.specs2
package data

import scalaz.stream.Process
import Process._
import scalaz.concurrent.Task
import Task._
import scalaz.Monoid
import scalaz.std.list._
import scalaz.syntax.foldable._
import scalaz.syntax.bind._
import scalaz.stream.io._

/**
 * A Fold[T] can be used to pass over a Process[Task, T].
 * 
 * It has:
 *
 *  - accumulation, with an initial state, of type S, a fold action and an action to perform with the last state
 *  
 *  - side-effects with a Sink[Task, (T, S)] to write to a file for example, using the current element in the Process
 *    and the current accumulated state
 *
 * This covers many of the needs of iterating over a Scalaz stream and is composable because there is a Monoid
 * instance for Folds
 * 
 */
trait Fold[T] {
  type S

  def prepare: Task[Unit]
  def sink: Sink[Task, (T, S)]
  def fold: (T, S) => S
  def init: S
  def last(s: S): Task[Unit]

}

/**
 * Fold functions and typeclasses
 */
object Fold {

  /**
   * Create a Fold from a Sink with no accumulation
   */
  def fromSink[T](aSink: Sink[Task, T]) =  new Fold[T] {
    type S = Unit
    lazy val sink: Sink[Task, (T, S)] = toFoldSink(aSink)

    def prepare = Task.now(())
    def fold = (t: T, u: Unit) => u
    def init = ()
    def last(u: Unit) = Task.now(u)
  }

  /**
   * Transform a simple sink into a sink, where the written value doesn't depend on the
   * current state to a sink for folds, where the current state is passed all the time
   * (and actually ignored here)
   */
  def toFoldSink[T, S](sink: Sink[Task, T]): Sink[Task, (T, S)] =
    sink.map(f => (ts: (T, S)) => f(ts._1))

  /**
   * Create a Fold from a side-effecting function
   */
  def fromFunction[T](f: T => Task[Unit]) =
    fromSink(Process.constant(f))

  /**
   * This Sink doesn't do anything
   * It can be used to build a Fold that does accumulation only
   */
  def unitSink[T, S]: Sink[Task, (T, S)] = channel((tu: (T, S)) => Task.now(()))

  /**
   * Unit Fold with no side-effect or accumulation
   */
  def unit[T] = fromSink(channel((t: T) => Task.now(())))

  /**
   * Accumulate state on a Process[Task, T] using an accumulation action and
   * an initial state
   */
  def foldState[S, T](action: (T, S) => S)(init: S): Process1[T, S] = {

    def go(state: S): Process1[T, S] =
      Process.receive1 { t: T =>
        val newState = action(t, state)
        emit(newState) fby go(newState)
      }

    go(init)
  }

  /**
   * Accumulate state on a Process[Task, T] using a Fold
   */
  def foldState[T](fold: Fold[T]): Process1[T, fold.S] =
    foldState(fold.fold)(fold.init)

  /**
   * Accumulate state on a Process[Task, T] using an accumulation action and
   * an initial state, but also keep the current element
   */
  def zipWithFoldState[S, T](action: (T, S) => S)(init: S): Process1[T, (T, S)] = {

    def go(state: S): Process1[T, (T, S)] =
      Process.receive1 { t: T =>
        val newState = action(t, state)
        emit((t, newState)) fby go(newState)
      }

    go(init)
  }

  /**
   * Accumulate state on a Process[Task, T] using a Fold
   */
  def zipWithFoldState[T](fold: Fold[T]): Process1[T, (T, fold.S)] =
    zipWithFoldState(fold.fold)(fold.init)

  /**
   * Monoid for Folds, where effects are sequenced
   */
  implicit def foldMonoid[T]: Monoid[Fold[T]] = new Monoid[Fold[T]] {
    def append(f1: Fold[T], f2: =>Fold[T]): Fold[T] = f1 >> f2
    lazy val zero = Fold.unit[T]
  }

  /** zip 2 state-folding functions together */
  implicit class zipFoldFunctions[T, S1](f1: (T, S1) => S1) {
    def zip[S2](f2: (T, S2) => S2): (T, (S1, S2)) => (S1, S2) = { (t: T, s12: (S1, S2)) =>
      val (s1, s2) = s12
      (f1(t, s1), f2(t, s2))
    }
  }

  /**
   * create a new Fold sequencing the effects of 2 Folds
   */
  implicit class sequenceFolds[T](val fold1: Fold[T]) {
    def >>(fold2: Fold[T]) = new Fold[T] {
      type S = (fold1.S, fold2.S)

      def prepare = fold1.prepare >> fold2.prepare

      def sink = fold1.sink.zipWith(fold2.sink) { (f1: ((T, fold1.S)) => Task[Unit], f2: ((T, fold2.S)) => Task[Unit]) =>
        (ts: (T, S)) => {
          val (t, (s1, s2)) = ts
          (f1((t, s1)) |@| f2((t, s2)))((_,_))
        }
      }

      def fold = (t : T, s12: (fold1.S, fold2.S)) => (fold1.fold(t, s12._1), fold2.fold(t, s12._2))
      def last(s12: (fold1.S, fold2.S)) = (fold1.last(s12._1) |@| fold2.last(s12._2))((_,_))
      def init = (fold1.init, fold2.init)
    }
  }

  /**
   * Run a fold an return the last value
   */
  def runFoldLast[T](process: Process[Task, T], fold: Fold[T]): Task[fold.S] =
    fold.prepare >>
    logged(process |> zipWithFoldState(fold)).drainW(fold.sink).map(_._2).runLastOr(fold.init)

  /**
   * Run a Fold an let it perform a last action with the accumulated state
   */
  def runFold[T](process: Process[Task, T], fold: Fold[T]): Task[Unit] =
    runFoldLast(process, fold).flatMap(fold.last)

  /**
   * Run a list of Folds, sequenced with the Fold Monoid
   */
  def runFolds[T](process: Process[Task, T], folds: List[Fold[T]]): Task[Unit] =
    runFold(process, folds.suml)

}


