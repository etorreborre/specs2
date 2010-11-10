package org.specs2
package reporter

import main.Arguments
import control.Fold
import specification._
import StandardFragments._

/**
 * This fold computes the 'level' of a given fragment. It is used to indent Fragments in
 * a ConsoleReporter and to create a tree or Descriptions in the JUnit runner
 * 
 * It does so by considering that:
 * 
 * * when a Text fragment follows a Text fragment we're going up one
 *   (so the second text fragment can be indented relatively to the first one)
 * * when an Example fragment follows a Text fragment we're going up one
 * * when an Example fragment follows an Example fragment we're staying on the same level
 * * when a Text fragment follows an Example fragment we're going down one level
 *
 * There are some cases though where the user would have to explicitly reset the level by
 * inserting an 'end' marker:
 * 
 * "this block"^
 *   "has 1 example" ! { true }^
 * "this other block"^
 *   "has a nested text"^
 *     "with 1 example" ! { true }^
 *     end^
 * "this third block"^
 *   "will not be nested thanks to the previous end marker" ! { true }^
 *
 *  The BlockLevelsFold trait is generic and specialized for:
 *    * Fragments => LevelsFold
 *    * ExecutedFragments => ExecutedLevelsFold
 *    
 */
private[specs2]
trait BlockLevelsFold[F] extends Fold[F] {

  type T = Level
  case class Level(level: Int = 0, direction: Direction = Up, lastNode: LastNode = Indent)
  lazy val initial = new Level()
  
  def toBlock(f: F): Block
  /**
   * This returns a function which computes the current level and the next Level object
   * storing the current state
   */
  def level(implicit arguments: Arguments): Function2[Level, F, (Int, Level)] = 
    (t: T, f: F) => (t, f) match {
      case p => (currentLevel(p), fold.tupled(p))
    }
  
  /**
   * This function computes the current level for a given Fragment
   */
  val currentLevel: Function[(Level, F), Int] = { 
    case (a, BlockIndent()) if (a.direction == Down && a.lastNode == Terminal) => (a.level - 1)
    case (a, f) => a.level
  }
  
  def fold(implicit arguments: Arguments) = (t: T, f: F) => (t, toBlock(f)) match {
    // end resets the level
    case (a, BlockReset()) => Level()
    case (a, BlockIndent()) => {
      val newLevel = a.direction match {
        case Up => a.copy(level = a.level + 1)
        case Down if (a.lastNode != Terminal) => a.copy(level = a.level + 1, direction = Up)
        case _ => a
      } 
      newLevel copy (lastNode = Indent)
    }
    case (a, BlockUnindent()) => {
      val newLevel = a.direction match {
        case Down => a.copy(level = a.level - 1)
        case Up if (a.lastNode != Terminal) => a.copy(level = a.level - 1, direction = Up)
        case _ => a
      } 
      newLevel copy (lastNode = Indent)
    }
    case (a, BlockTerminal()) => a.copy(direction = Down, lastNode = Terminal)
    case (t, BlockNeutral()) => t
  }
 
}
private[specs2]
sealed trait Block
case class BlockReset() extends Block
case class BlockIndent() extends Block
case class BlockUnindent() extends Block
case class BlockTerminal() extends Block
case class BlockNeutral() extends Block

private[specs2]
sealed trait Direction
case object Up extends Direction
case object Down extends Direction

private[specs2]
sealed trait LastNode
case object Terminal extends LastNode
case object Indent extends LastNode

private[specs2]
trait LevelsFold extends FragmentFold {
  val blockFold = new BlockLevelsFold[Fragment] {
    def toBlock(f: Fragment) = f match {
      case Example(_, _) => BlockTerminal()
      case Tab()         => BlockIndent()
      case Backtab()       => BlockUnindent()
      case Text(_)       => BlockIndent()
      case SpecStart(_, _)  => BlockReset()
      case SpecEnd(_)    => BlockReset()
      case End()         => BlockReset()
      case _             => BlockNeutral()
    }
  }
  type T = blockFold.Level
  type Level = blockFold.Level
  def initial = blockFold.initial
  def fold(implicit args: Arguments) = blockFold.fold(args)

}
private[specs2]
object LevelsFold extends LevelsFold

private[specs2]
trait ExecutedLevelsFold extends ExecutedFragmentFold {
  
  val blockFold = new BlockLevelsFold[ExecutedFragment] {
    def toBlock(f: ExecutedFragment) = f match {
      case ExecutedResult(_, _)       => BlockTerminal()
      case ExecutedText(_)            => BlockIndent()
      case ExecutedTab()              => BlockIndent()
      case ExecutedBacktab()            => BlockUnindent()
      case ExecutedSpecStart(_, _, _) => BlockReset()
      case ExecutedSpecEnd(_)         => BlockReset()
      case ExecutedEnd()              => BlockReset()
      case _                          => BlockNeutral()
    }
  }
  type T = blockFold.Level
  def initial = blockFold.initial
  def fold(implicit args: Arguments) = blockFold.fold(args)
}
