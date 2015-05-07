package org.specs2
package foldm

import scala.annotation.tailrec
import scalaz.{EphemeralStream, Bind, ~>, Foldable, \/, \/-, -\/}
import EphemeralStream._
import scalaz.syntax.bind._
import scalaz.syntax.foldable._
import scalaz.std.list._

/**
 * A structure delivering elements of type A (variable type, like a List) and which 
 * can be folded over
 */
trait FoldableM[F[_], M[_]]  { self =>
  def foldM[A, B](fa: F[A])(fd: FoldM[A, M, B]): M[B]

  def foldMBreak[A, B](fa: F[A])(fd: FoldM[A, M, B] { type S = B \/ B }): M[B]

  def into[G[_]](implicit nat: G ~> F): FoldableM[G, M] = new FoldableM[G, M] {
    def foldM[A, B](fa: G[A])(fd: FoldM[A, M, B]): M[B] =
      self.foldM(nat(fa))(fd)

    def foldMBreak[A, B](fa: G[A])(fd: FoldM[A, M, B] { type S = B \/ B }): M[B] =
      self.foldMBreak(nat(fa))(fd)
  }
}

object FoldableM {

  def apply[F[_], M[_]](implicit fm: FoldableM[F, M]): FoldableM[F, M] =
    implicitly[FoldableM[F, M]]

  implicit def IteratorIsFoldableM[M[_] : Bind]: FoldableM[Iterator, M] =  new FoldableM[Iterator, M] {
    def foldM[A, B](iterator: Iterator[A])(fd: FoldM[A, M, B]): M[B] =
      fd.start.flatMap { st =>
        var state = st
        while (iterator.hasNext)
          state = fd.fold(state, iterator.next)
        fd.end(state)
      }

    def foldMBreak[A, B](iterator: Iterator[A])(fd: FoldM[A, M, B] { type S = B \/ B }): M[B] = {
      @tailrec
      def foldState(it: Iterator[A], state: fd.S): fd.S =
        if (it.hasNext)
          fd.fold(state, it.next) match {
            case \/-(stop)     => \/-(stop)
            case -\/(continue) => foldState(it, -\/(continue))
          }
        else state

      fd.start.flatMap(st => fd.end(foldState(iterator, st)))
    }

  }

  implicit def FoldableIsFoldableM[F[_] : Foldable, M[_] : Bind]: FoldableM[F, M] = new FoldableM[F, M] {
    def foldM[A, B](fa: F[A])(fd: FoldM[A, M, B]): M[B] =
      fd.start.flatMap(st => fd.end(fa.foldLeft(st)(fd.fold)))

    def foldMBreak[A, B](fa: F[A])(fd: FoldM[A, M, B] { type S = B \/ B }): M[B] = {
      @tailrec
      def foldState(stream: EphemeralStream[A], state: fd.S): fd.S =
        stream match {
          case head ##:: tail =>
            fd.fold(state, head) match {
              case \/-(stop)     => \/-(stop)
              case -\/(continue) => foldState(tail, -\/(continue))
            }
          case _ => state
        }

      fd.start.flatMap(st => fd.end(foldState(fa.toEphemeralStream, st)))
    }
  }

}
