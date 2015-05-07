package org.specs2
package foldm

import scala.io.BufferedSource
import scalaz.{\/, \/-, -\/, Bind, ~>}
import scalaz.syntax.bind._
import java.io.InputStream
import FoldId.Bytes

/**
 * A structure delivering elements of type A (fixed type, like an InputStream) and which 
 * can be folded over
 */
trait FoldableMS[A, F, M[_]]  { self =>
  def foldM[B](fa: F)(fd: FoldM[A, M, B]): M[B]
  def foldMBreak[B](fa: F)(fd: FoldM[A, M, B] {type S = B \/ B }): M[B]
}

object FoldableMS {

  implicit def BufferedSourceIsFoldableMS[S <: BufferedSource, M[_] : Bind]: FoldableMS[String, S, M] = new FoldableMS[String, S, M] {
    def foldM[B](s: S)(fd: FoldM[String, M, B]): M[B] =
      FoldableM.IteratorIsFoldableM.foldM(s.getLines)(fd)

    def foldMBreak[B](s: S)(fd: FoldM[String, M, B] {type S = B \/ B }): M[B] =
      FoldableM.IteratorIsFoldableM.foldMBreak(s.getLines)(fd)
  }

  implicit def InputStreamIsFoldableMS[IS <: InputStream, M[_] : Bind]: FoldableMS[Bytes, IS, M] =
    inputStreamAsFoldableMS(bufferSize = 4096)

  def inputStreamAsFoldableMS[IS <: InputStream, M[_] : Bind](bufferSize: Int): FoldableMS[Bytes, IS, M] = new FoldableMS[Bytes, IS, M] {
    def foldM[B](is: IS)(fd: FoldM[Bytes, M, B]): M[B] =
      fd.start.flatMap { st =>
        val buffer = Array.ofDim[Byte](bufferSize)
        var length = 0
        var state = st
        while ({ length = is.read(buffer, 0, buffer.length); length != -1 })
          state = fd.fold(state, (buffer, length))
        fd.end(state)
      }

    def foldMBreak[B](is: IS)(fd: FoldM[Bytes, M, B] {type S = B \/ B }): M[B] =
      fd.start.flatMap { st =>
        val buffer = Array.ofDim[Byte](bufferSize)
        var length = 0
        var state = st
        var break = false
        while ({ length = is.read(buffer, 0, buffer.length); length != -1 && !break }) {
          state = fd.fold(state, (buffer, length))
          state match {
            case \/-(s) => break = true
            case -\/(s) => ()
          }
        }
        fd.end(state)
      }
  }

}
