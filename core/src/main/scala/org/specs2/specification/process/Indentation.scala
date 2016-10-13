package org.specs2
package specification
package process

import scala.math._
import control.origami._, Folds._
import specification.core._

/**
 * Fold function to compute the indentation of each fragment based
 * on the presence of Tabs fragments
 */
trait Indentation {

  def foldLeft = (indentation: Int, fragment: Fragment) => {
    fragment match {
      case f @ Fragment(Tab(n),_ ,_)     => indentation + n
      case f @ Fragment(Backtab(n),_ ,_) => max(0, indentation - n)
      case _                             => indentation
    }
  }

  def foldLeftIndentationState = (indentation: IndentationState, fragment: Fragment) =>
    fragment match {
      case f @ Fragment(Tab(n),_ ,_)     => indentation.copy(indentation.level + 1, IndentationUp)
      case f @ Fragment(Backtab(n),_ ,_) => indentation.copy(max(0, indentation.level - 1), IndentationDown)
      case _                             => indentation
    }

  def fold: FoldState[Fragment, Int] =
    fromFoldLeft(0)(foldLeft)

  def foldIndentationState: FoldState[Fragment, IndentationState] =
    fromFoldLeft(IndentationState.empty)(foldLeftIndentationState)
}

object Indentation extends Indentation

case class IndentationState(level: Int, direction: IndentationDirection) {
  def isUp = direction == IndentationUp
  def isDown = direction == IndentationDown
}
object IndentationState {
  val empty = IndentationState(level= 0, direction = IndentationNeutral)
}

sealed trait IndentationDirection
case object IndentationDown extends IndentationDirection
case object IndentationUp extends IndentationDirection
case object IndentationNeutral extends IndentationDirection
