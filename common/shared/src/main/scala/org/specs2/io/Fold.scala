package org.specs2
package io

import java.io.PrintWriter

import control._
import origami._
import fp.Show
import fp.syntax._

object FoldIo {

  /** create a fold sink to output lines to a file */
  def showToFilePath[T : Show](path: FilePath): Sink[Action, T] =
    printToFilePath(path)(t => Action.pure(Show[T].show(t)))

  /** create a fold sink to output lines to a file */
  def printToFilePath[T](path: FilePath)(print: T => Action[String]): Sink[Action, T] =
    Folds.bracket(Action.pure(new PrintWriter(path.path)))(
      (p: PrintWriter, t: T) => print(t).map(p.write).as(p))(
      (p: PrintWriter) => Finalizer(() => p.close()))
}
