package org.specs2
package specification
package dsl
package mutable

import org.specs2.control.{Throwablex, Throwables}
import org.specs2.execute.ExecuteException
import Throwablex._
import scala.util.control.NonFatal
import scalaz.TreeLoc
import scalaz.Tree._

/**
 * This class tracks "nested" effects.
 *
 * It is used to create nested blocks in mutable specifications and make sure
 * that we can control which blocks to evaluate based on a target "path" in the tree
 * of blocks
 */
private[specs2]
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
  private val effects: scala.collection.mutable.ListBuffer[() => Unit] = new scala.collection.mutable.ListBuffer()

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

  private def startBlock() = effect {
    blocksTree = blocksTree.insertDownLast(nextNodeNumber)
  }

  private def endBlock() = effect {
    blocksTree = blocksTree.getParent
  }

  def isAt(path: Option[EffectPath]) = path.toList.contains(effectPath)

  def nestBlock(block: =>Any) = {
    startBlock()
    block
    endBlock()
  }

  /** stop creating fragments when the target is reached */
  def stopEffects() = stop = true


  def addBlock[T](t: =>T) = effect {
    blocksTree = blocksTree.addChild(nextNodeNumber)
    try {
      t
    } catch {
      case e: ExecuteException => throw SpecificationCreationExpectationException(e)
      case NonFatal(e)         => throw SpecificationCreationException(e)
    }
    ()
  }

  def effect(a: =>Unit) =
    effects.append(() => a)

  implicit class TreeLocx[T](t: TreeLoc[T]) {
    def getParent = t.parent.getOrElse(t)
    def addChild(c: T) = t.insertDownLast(leaf(c)).getParent
    def insertDownLast(c: T) = t.insertDownLast(leaf(c))
  }
}

case class SpecificationCreationExpectationException(t: ExecuteException) extends Exception {
  override def getMessage: String =
    s"""|
        |An expectation was executed during the creation of the specification: ${t.getMessage}.
        |This means that you have some code which should be enclosed in an example. Instead of writing:
        |
        | "this is a block of examples" in {
        |   // test something
        |   1 must_== 2
        |   "first example" in { 1 must_== 1 }
        |   "second example" in { 1 must_== 1 }
        | }
        |
        |You should write:
        |
        | "this is a block of examples" in {
        |   "example zero" in {
        |     // test something
        |     1 must_== 2
        |   }
        |   "first example" in { 1 must_== 1 }
        |   "second example" in { 1 must_== 1 }
        | }
        |
        |EXCEPTION
        |
        |${Throwables.renderWithStack(t)}
        |
        |CAUSED BY
        |
        |${t.chainedExceptions.map(Throwables.renderWithStack).mkString("\n")}
        |
        |""".stripMargin
}


case class SpecificationCreationException(t: Throwable) extends Exception {
  override def getMessage: String =
    s"""|
        |An exception was raised during the creation of the specification: ${t.getMessage}.
        |This means that you have some code which should be enclosed in an example. Instead of writing:
        |
        | "this is a block of examples" in {
        |   // set-up something
        |   createDatabase
        |   "first example" in { 1 must_== 1 }
        |   "second example" in { 1 must_== 1 }
        | }
        |
        |You should write:
        |
        | "this is a block of examples" in {
        |   "the setup must be ok" in {
        |     createDatabase must not(throwAn[Exception])
        |   }
        |   "first example" in { 1 must_== 1 }
        |   "second example" in { 1 must_== 1 }
        | }
        |
        | Be careful because in the specification above the expectation might be that the
        | database will be created before the "first" and "second" examples. This will NOT be the case
        | unless you mark the specification as `sequential`. You can also have a look at the `BeforeEach/BeforeAll` traits
        | to implement this kind of functionality.
        |
        |EXCEPTION
        |
        |${Throwables.renderWithStack(t)}
        |
        |CAUSED BY
        |
        |${t.chainedExceptions.map(Throwables.renderWithStack).mkString("\n")}
        |
        |""".stripMargin
}

private[specs2]
case class EffectPath(path: Seq[Int] = Seq()) {
  def startsWith(other: EffectPath) = path.take(other.path.size) == other.path
}


