package org.specs2
package data

import io.FilePath

import scalaz.stream._
import Process._
import scalaz.concurrent.Task
import Task._
import scalaz.{Show, Reducer, Monoid}
import scalaz.std.list._
import scalaz.syntax.foldable._
import scalaz.syntax.bind._
import scalaz.stream._
import io._
import scalaz.stream.text._
import Processes._
import process1.lift
import control.Functions._

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

  /** create a Process1 returning the state values */
  def foldState1: Process1[T, S] =
    Processes.foldState1(fold)(init)

  /** create a Process1 returning the folded elements and the state values */
  def zipWithState1: Process1[T, (T, S)] =
    Processes.zipWithState1(fold)(init)

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
    lazy val sink: Sink[Task, (T, S)] = aSink.extend[S]

    def prepare = Task.now(())
    def fold = (t: T, u: Unit) => u
    def init = ()
    def last(u: Unit) = Task.now(u)
  }

  /**
   * Transform a simple sink where the written value doesn't depend on the
   * current state into a sink where the current state is passed all the time
   * (and actually ignored)
   * Create a Fold a State function
   */
  def fromState[T, S1](state: (T, S1) => S1)(initial: S1) = new Fold[T] {
    type S = S1
    lazy val sink: Sink[Task, (T, S)] = unitSink[T, S]

    def prepare = Task.now(())
    def fold = state
    def init = initial
    def last(s: S) = Task.now(())
  }

  /**
   * Create a Fold from a side-effecting function
   */
  def fromFunction[T](f: T => Task[Unit]): Fold[T] =
    fromSink(Process.constant(f))

  /**
   * Create a Fold from a Reducer
   */
  def fromReducer[T, S1](reducer: Reducer[T, S1]): Fold[T] = new Fold[T] {
    type S = S1
    lazy val sink: Sink[Task, (T, S)] = unitSink[T, S]

    def prepare = Task.now(())
    def fold = reducer.cons
    def init = reducer.monoid.zero
    def last(s: S) = Task.now(())
  }

  /**
   * Create a Fold from a Reducer and a last action
   */
  def fromReducerAndLast[T, S1](reducer: Reducer[T, S1], lastTask: S1 => Task[Unit]): Fold[T] = new Fold[T] {
    type S = S1
    lazy val sink: Sink[Task, (T, S)] = unitSink[T, S]

    def prepare = Task.now(())
    def fold = reducer.cons
    def init = reducer.monoid.zero
    def last(s: S) = lastTask(s)
  }

  /**
   * This Sink doesn't do anything
   * It can be used to build a Fold that does accumulation only
   */
  def unitSink[T, S]: Sink[Task, (T, S)] =
    channel((tu: (T, S)) => Task.now(()))

  /**
   * Unit Fold with no side-effect or accumulation
   */
  def unit[T] = fromSink(channel((t: T) => Task.now(())))

  /**
   * Unit fold function
   */
  def unitFoldFunction[T]: (T, Unit) => Unit = (t: T, u: Unit) => u

  /** create a fold sink to output lines to a file */
  def showToFilePath[T : Show, S](path: FilePath): Sink[Task, (T, S)] =
    nio.file.chunkW(path.path).pipeIn(lift(Show[T].shows) |> utf8Encode).extend[S]

  /**
   * Monoid for Folds, where effects are sequenced
   */
  implicit def foldMonoid[T]: Monoid[Fold[T]] = new Monoid[Fold[T]] {
    def append(f1: Fold[T], f2: =>Fold[T]): Fold[T] = f1 >> f2
    lazy val zero = Fold.unit[T]
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
    logged(process |> fold.zipWithState1).drainW(fold.sink).map(_._2).runLastOr(fold.init)

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


