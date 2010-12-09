package org.specs2
package reporter

import scalaz._
import Scalaz._
import scala.math._
import main.Arguments
import specification._
import data.Trees._
import StandardFragments._

/**
 * This class computes the 'level' of a given fragment. It is used to indent Fragments in
 * a ConsoleReporter and to create a tree or Descriptions in the JUnit runner
 * 
 * It does so by considering that:
 * 
 * * when a Text fragment follows a Text fragment we're going up one level
 *   (so the second text fragment can be indented relatively to the first one)
 * * when an Example fragment follows a Text fragment we're going up one level
 * * when an Example fragment follows an Example fragment we're staying on the same level
 * * when a Text fragment follows an Example fragment we're going down one level
 * * when a paragraph follows anything, we're going down one level
 * * when there is a end, we reset the levels to zero
 * * when there is a tab we indent everything following the tab (1 level is the default)
 * * when there is a backtab we unindent everything following the tab (1 level is the default)
 * 
 */
case class Levels[T](blocks: List[(Block[T], Int)] = Nil) {
  /** @return the first block */
  private def headOption = blocks.map(_._1).headOption
  /** @return the last block */
  private def lastOption = blocks.map(_._1).lastOption
  /** @return the first level or zero */
  private def firstLevel = blocks.map(_._2).headOption.getOrElse(0)
  /** @return the last level or zero */
  private def lastLevel = blocks.map(_._2).lastOption.getOrElse(0)
  /** @return alias for the last level */
  def level = levels.lastOption.getOrElse(0)
  /** @return all the levels, post-processing them so that there is no negative value */
  def levels = {
    val minLevel = blocks.map(_._2).min
	  blocks.map(_._2 - min(0, minLevel))
  }

  
  /** @return the concatenation of 2 levels */
  def add(other: Levels[T]) = Levels(this.blocks ++ other.blocks)
  /** 
   * @return reset the levels of all blocks by incrementing or decrementing the level 
   *         value, until a Reset block is met
   */
  def resetLevel(f: Int => Int) = {
    val breakAtFirstReset = blocks.span(b => !isReset(b)) 
    Levels(breakAtFirstReset._1).mapLevel(f) add 
    Levels(breakAtFirstReset._2) 
  }
  /** 
   * @return reset all the levels of all blocks by incrementing or decrementing the level 
   *         value
   */
  private def mapLevel(f: Int => Int) = Levels(blocks.map((b: (Block[T], Int)) => (b._1, f(b._2))))

  /**
   * @return a Tree[T] based on the level of each block
   */
  def toTree: Tree[T] = toTreeLoc.toTree
  /**
   * @return a Tree[S] based on the level of each block, mapping each node to value of type
   *         S and possibly skipping nodes
   */
  def toTree[S](m: (T, Int) => Option[S]): Tree[S] = toTreeLoc(m).toTree

  /**
   * @return a TreeLoc[T] based on the level of each block
   */
  def toTreeLoc: TreeLoc[T] = toTreeLoc((t:T, i: Int) => Some(t))
  /**
   * WARNING this method assumes that the Levels are not empty!!
   * 
   * @return a Tree[S] based on the level of each block, mapping each node to value of type
   *         S and possibly skipping nodes, passing the numeric label of the current node. 
   * @see JUnitDescriptions
   */
  def toTreeLoc[S](m: (T, Int) => Option[S]): TreeLoc[S] = {
    val initial = m(blocks.head._1.t, 0).get
    blocks.drop(1).foldLeft(leaf(initial).loc) { (treeLoc, cur) =>
      val (block, level) = cur
      m(block.t, treeLoc.root.toTree.flatten.size) match {
        case Some(s) =>
          treeLoc.parentLocs.drop(level).headOption.getOrElse(treeLoc).insertDownLast(leaf(s))
        case None =>  treeLoc
      }
    }
  } 
  /** @return true if a Block is a Reset block */
  private val isReset = (b: (Block[T], Int)) => b match { case (BlockReset(t), _) => true; case _ => false }
  override def equals(a: Any) = {
    a match {
      case l: Levels[_] => normalizeResets.blocks.equals(l.normalizeResets.blocks)
      case _ => false
    }
  }
  /**
   * normalize resets so that BlockReset levels will not be compared
   */
  private def normalizeResets = {
    new Levels(blocks.map {
      case (BlockReset(t), _) => (BlockReset(t), 0) 
      case other              => other 
    })
  }
}
case object Levels {
  /** @return a new Levels object for one Block */
  def apply[T](b: Block[T]) = new Levels(List((b, 0)))
  /** monoid for Levels */
  implicit def LevelsMonoid[T] = new Monoid[Levels[T]] {
    def append(b1: Levels[T], b2: =>Levels[T]) =
      (b1.lastOption, b2.headOption) match {
        case (None, _)                        => b2
        case (Some(BlockReset(t)), _)         => b1 add b2.resetLevel(_ - b2.firstLevel)
        case (_, Some(BlockReset(t)))         => b1 add b2.resetLevel(_ - b2.firstLevel)
        case (Some(BlockIndent(t, n)), _)     => b1 add b2.resetLevel(b1.lastLevel + _ + n)
        case (Some(BlockUnindent(t, n)), _)   => b1 add b2.resetLevel(b1.lastLevel + _ - n)
        case _                                => b1 add b2.resetLevel(b1.lastLevel + _)
      }
    val zero = new Levels[T]()
  }
  /** fold a list of T to a Levels object */
  def foldAll[T](fs: Seq[T])(implicit reducer: Reducer[T, Levels[T]]) = {
    fs.foldMap(reducer.unit)
  }
  implicit object LevelsReducer extends Reducer[ExecutedFragment, Levels[ExecutedFragment]] {
    implicit def toBlock(f: ExecutedFragment): Block[ExecutedFragment] = f match {
      case t @ ExecutedResult(_, _)       => BlockTerminal(t) 
      case t @ ExecutedText(_)            => BlockIndent(t)   
      case t @ ExecutedPar()              => BlockUnindent(t)   
      case t @ ExecutedTab(n)             => BlockIndent(t, n)   
      case t @ ExecutedBacktab(n)         => BlockUnindent(t, n) 
      case t @ ExecutedSpecStart(_, _, _) => BlockReset(t)    
      case t @ ExecutedSpecEnd(_)         => BlockReset(t)    
      case t @ ExecutedEnd()              => BlockReset(t)    
      case t                              => BlockNeutral(t)  
    } 
    implicit override def unit(f: ExecutedFragment): Levels[ExecutedFragment] = Levels[ExecutedFragment](toBlock(f))
    
  }
  implicit object FragmentLevelsReducer extends Reducer[Fragment, Levels[Fragment]] {
    implicit def toBlock(f: Fragment): Block[Fragment] = f match {
      case t @ Example(_, _)   => BlockTerminal(t)     
      case t @ Par()           => BlockUnindent(t)   
      case t @ Tab(n)          => BlockIndent(t, n)       
      case t @ Backtab(n)      => BlockUnindent(t, n)   
      case t @ Text(_)         => BlockIndent(t)       
      case t @ SpecStart(_, _) => BlockReset(t)     
      case t @ SpecEnd(_)      => BlockReset(t)        
      case t @ End()           => BlockReset(t)        
      case t                   => BlockNeutral(t)        
    }
    implicit override def unit(f: Fragment): Levels[Fragment] = Levels(toBlock(f))
  }
}
/** this represent a fragment of a specification that needs to be indented as a block */
sealed trait Block[T] {
  val t: T
}
case class BlockTerminal[T](t: T = null) extends Block[T] 
case class BlockIndent[T](t: T = null, n: Int = 1)  extends Block[T]
case class BlockUnindent[T](t: T = null, n: Int = 1) extends Block[T]
case class BlockReset[T](t: T = null)    extends Block[T]
case class BlockNeutral[T](t: T = null)  extends Block[T]