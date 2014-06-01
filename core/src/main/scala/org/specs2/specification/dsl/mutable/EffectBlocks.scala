package org.specs2
package specification
package dsl
package mutable

import scalaz.TreeLoc
import scalaz.Tree._

/**
 * This class tracks "nested" effects
 */
class EffectBlocks {
  /**
   * This tree loc contains the "path" of Examples and Actions when they are created in a block creating another fragment
   * For example:
   *
   * "this" should {
   *   "create an example" >> ok
   * }
   *
   * The execution of the block above creates a Text fragment followed by an example. The blocksTree tree tracks the order of creation
   * so that we can attach a "block creation path" to the Example showing which fragment creation precedes him. This knowledge is used to
   * run a specification in the "isolation" mode were the changes in local variables belonging to blocks are not seen by
   * other examples
   */
  private[specs2] var blocksTree: TreeLoc[Int] = leaf(0).loc

  /** list of actions to create fragments */
  private var effects: scala.collection.mutable.ListBuffer[() => Unit] = new scala.collection.mutable.ListBuffer()

  /** stop evaluating effects */
  private var stop = false

  /** get the node number on the current branch and add 1 */
  private def nextNodeNumber = blocksTree.lastChild.map(_.getLabel + 1).getOrElse(0)

  /**
   * Play all effects.
   *
   * After each executed effect, new effects might have been created.
   * Push them first at the beginning of the effects list so that they can be played first.
   *
   * This is in particular the case when the effects are
   *
   *  - open block
   *  - spawn new effects
   *  - close block
   *
   *  the effects will have to be executed before the block is closed
   *
   */
  lazy val replay = {

    while (!effects.isEmpty && !stop) {
      val (effect, rest) = (effects.head, effects.tail)
      effects.clear

      effect.apply()
      effects.append(rest:_*)
    }
  }

  /** @return the current path to root */
  def effectPath = EffectPath(blocksTree.lastChild.getOrElse(blocksTree).path.reverse.toIndexedSeq)

  private def startBlock = effect {
    blocksTree = blocksTree.insertDownLast(nextNodeNumber)
  }

  private def endBlock = effect {
    blocksTree = blocksTree.getParent
  }

  def isAt(path: Option[EffectPath]) = path.exists(_ == effectPath)

  def nestBlock(block: =>Any) = {
    startBlock
    block
    endBlock
  }

  /** stop creating fragments when the target is reached */
  def stopEffects = stop = true


  def addBlock[T](t: =>T) = effect {
    blocksTree = blocksTree.addChild(nextNodeNumber)
    t
  }

  def effect(a: =>Unit) =
    effects.append(() => a)

  implicit class TreeLocx[T](t: TreeLoc[T]) {
    def getParent = t.parent.getOrElse(t)
    def addChild(c: T) = t.insertDownLast(leaf(c)).getParent
    def insertDownLast(c: T) = t.insertDownLast(leaf(c))
  }
}

case class EffectPath(path: Seq[Int] = Seq()) {
  def startsWith(other: EffectPath) = path.take(other.path.size) == other.path
}


