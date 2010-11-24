package org.specs2
package reporter

import scalaz._
import Scalaz._
import scala.math.max
import main.Arguments
import specification._
import StandardFragments._

/**
 * This class computes the 'level' of a given fragment. It is used to indent Fragments in
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
case class LeveledBlocks[T](blocks: List[(Block[T], Int)] = Nil) {
  def headOption = blocks.map(_._1).headOption
  def lastOption = blocks.map(_._1).lastOption
  def firstLevel = blocks.headOption.map(_._2).getOrElse(0)
  def lastLevelOption = blocks.map(_._2).lastOption
  def level = lastLevelOption.getOrElse(0)
  def levels = blocks.map(_._2)
  def elems = blocks.map(_._1)
  def add(other: LeveledBlocks[T]) = LeveledBlocks(this.blocks ++ other.blocks)
  def resetLevel(f: Int => Int) = {
    val breakAtFirstReset = blocks.span(b => !isReset(b)) 
    LeveledBlocks(breakAtFirstReset._1).mapLevel(f) add 
    LeveledBlocks(breakAtFirstReset._2) 
  }
  private def mapLevel(f: Int => Int) = LeveledBlocks(blocks.map((b: (Block[T], Int)) => (b._1, f(b._2))))
  def increment(n: Int = 1) = mapLevel(_ + n)
  def decrement(n: Int = 1) = mapLevel(_ - n)

  def toTree: Tree[T] = toTreeLoc.toTree
  def toTree[S](m: (T, Int) => Option[S]): Tree[S] = toTreeLoc(m).toTree

  def toTreeLoc: TreeLoc[T] = toTreeLoc((t:T, i: Int) => Some(t))
  def toTreeLoc[S](m: (T, Int) => Option[S]): TreeLoc[S] = {
    val bb = blocks
    val initial = m(blocks.head._1.t, 0).get
    blocks.drop(1).foldLeft(leaf(initial).loc) { (treeLoc, cur) =>
      val (block, level) = cur
      m(block.t, treeLoc.root.toTree.flatten.size) match {
        case Some(s) =>
          parentLocs(treeLoc).drop(level).headOption.getOrElse(treeLoc).insertDownLast(leaf(s))
        case None =>
          treeLoc
      }
    }
  } 
  private def parentLocs[A](t: TreeLoc[A], ps: List[TreeLoc[A]] = Nil): List[TreeLoc[A]] = t.parent match {
    case Some(p) => parentLocs(p, p :: ps)
    case None    => ps
  }
  
  private val isReset = (b: (Block[T], Int)) => b match { case (BlockReset(t), _) => true; case _ => false }
  override def equals(a: Any) = {
    a match {
      case l: LeveledBlocks[_] => normalizeResets.blocks.equals(l.normalizeResets.blocks)
      case _ => false
    }
  }
  private def normalizeResets = {
    new LeveledBlocks(blocks.map {
      case (BlockReset(t), _) => (BlockReset(t), 0) 
      case other              => other 
    })
  }
}
case object LeveledBlocks {
  def apply[T](b: Block[T]) = new LeveledBlocks(List((b, 0)))
  implicit def LeveledBlocksMonoid[T] = new Monoid[LeveledBlocks[T]] {
    def append(b1: LeveledBlocks[T], b2: =>LeveledBlocks[T]) =
      (b1.lastOption, b2.headOption) match {
        case (None, _)                        => b2
        case (Some(BlockReset(t)), _)         => b1 add b2.resetLevel(_ - b2.firstLevel)
        case (_, Some(BlockReset(t)))         => b1 add b2.resetLevel(_ - b2.firstLevel)
        case (Some(BlockIndent(t, n)), _)     => b1 add b2.resetLevel(b1.level + _ + n)
        case (Some(BlockUnindent(t, n)), _)   => b1 add b2.resetLevel(b1.level + _ - n)
        case _                                => b1 add b2.resetLevel(b1.level + _)
      }
    val zero = new LeveledBlocks[T]()
  }
  def foldAll[T](fs: Seq[T])(implicit reducer: Reducer[T, LeveledBlocks[T]]) = {
    fs.foldMap(reducer.unit)
  }
  implicit object ExecutedFragmentLeveledBlocksReducer extends Reducer[ExecutedFragment, LeveledBlocks[ExecutedFragment]] {
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
    implicit override def unit(f: ExecutedFragment): LeveledBlocks[ExecutedFragment] = LeveledBlocks[ExecutedFragment](toBlock(f))
    
  }
  implicit object FragmentLeveledBlocksReducer extends Reducer[Fragment, LeveledBlocks[Fragment]] {
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
    implicit override def unit(f: Fragment): LeveledBlocks[Fragment] = LeveledBlocks(toBlock(f))
  }
}
sealed trait Block[T] {
  val t: T
}
case class BlockTerminal[T](t: T = null) extends Block[T] 
case class BlockIndent[T](t: T = null, n: Int = 1)  extends Block[T]
case class BlockUnindent[T](t: T = null, n: Int = 1) extends Block[T]
case class BlockReset[T](t: T = null)    extends Block[T]
case class BlockNeutral[T](t: T = null)  extends Block[T]