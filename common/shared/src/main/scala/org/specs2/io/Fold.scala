package org.specs2
package io

import java.io.PrintWriter

import control._
import origami._
import eff.all._
import fp.Show
import fp.syntax._
import org.specs2.control.eff.Eff

object FoldIo {

  /** create a fold sink to output lines to a file */
  def showToFilePath[R :_Safe, T : Show](path: FilePath): Sink[Eff[R, ?], T] =
    printToFilePath(path)(t => Eff.pure(Show[T].show(t)))

  /** create a fold sink to output lines to a file */
  def printToFilePath[R :_Safe, T](path: FilePath)(print: T => Eff[R, String]): SinkEff[R, T] =
    Folds.bracket(protect(new PrintWriter(path.path)))(
      (p: PrintWriter, t: T) => print(t).map(p.write).as(p))(
      (p: PrintWriter) => protect(p.close))
}
