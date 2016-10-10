package org.specs2
package io

import java.io.PrintWriter

import control._
import origami._
import eff.all._

import scalaz.Show

object Fold {

  /** create a fold sink to output lines to a file */
  def showToFilePath[R :_Safe, T : Show](path: FilePath): Fold[R, T, Unit] =
    Folds.bracket(protect(new PrintWriter(path.path)))(
      (p: PrintWriter, t: T) => protect { p.write(Show[T].shows(t)); p })(
      (p: PrintWriter) => protect(p.close))

}
