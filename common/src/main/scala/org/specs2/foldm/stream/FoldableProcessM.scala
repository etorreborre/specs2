package org.specs2
package foldm
package stream

import scalaz.{\/, \/-, -\/, Monad, Catchable}
import scalaz.syntax.bind._
import scalaz.stream.{Process1, Process}
import scalaz.stream.Process._

/**
 * Foldable instance for Process[M, O]
 */
object FoldableProcessM {

  implicit def ProcessFoldableM[M[_]](implicit F: Monad[M], C: Catchable[M]): FoldableM[({type l[A]=Process[M, A]})#l, M] = new FoldableM[({type l[A]=Process[M, A]})#l, M] {
    def foldM[A, B](fa: Process[M, A])(fd: FoldM[A, M, B]): M[B] = {
      def go(state: fd.S): Process1[A, fd.S] =
        Process.receive1 { a: A =>
          val newState = fd.fold(state, a)
          emit(newState) fby go(newState)
        }

      fd.start.flatMap { st =>
        (fa |> go(st)).runLast.flatMap(last => fd.end(last.getOrElse(st)))
      }
    }

    def foldMBreak[A, B](fa: Process[M, A])(fd: FoldM[A, M, B] { type S = B \/ B }): M[B] = {
      def go(state: fd.S): Process1[A, fd.S] =
        Process.receive1 { a: A =>
          val newState = fd.fold(state, a)
          newState match {
            case \/-(s) => emit(newState)
            case -\/(s) => emit(newState) fby go(newState)
          }
        }

      fd.start.flatMap { st =>
        (fa |> go(st)).runLast.flatMap(last => fd.end(last.getOrElse(st)))
      }
    }
  }

}
