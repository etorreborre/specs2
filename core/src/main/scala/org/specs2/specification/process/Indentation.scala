package org.specs2
package specification
package process

import scala.math._
import specification.core._
import scalaz._, Scalaz._
/**
 * Fold function to compute the indentation of each fragment based
 * on the presence of Tabs fragments
 */
trait Indentation {

  def fold = (fragment: Fragment, indentation: Int) => {
    fragment match {
      case f @ Fragment(Tab(n),_ ,_)     => indentation + n
      case f @ Fragment(Backtab(n),_ ,_) => max(0, indentation - n)
      case _                             => indentation
    }
  }

  def foldIndentationState = (fragment: Fragment, indentation: IndentationState) =>
    fragment match {
      case f @ Fragment(Tab(n),_ ,_)     => indentation.copy(indentation.level + 1, IndentationUp)
      case f @ Fragment(Backtab(n),_ ,_) => indentation.copy(max(0, indentation.level - 1), IndentationDown)
      case _                             => indentation
    }

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
