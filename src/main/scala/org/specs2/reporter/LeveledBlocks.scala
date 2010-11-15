package org.specs2
package reporter

import scalaz._
import Scalaz._
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
case class LeveledBlocks(blocks: List[(Block, Int)] = Nil) {
  def headOption = blocks.map(_._1).headOption
  def lastOption = blocks.map(_._1).lastOption
  def firstLevel = blocks.headOption.map(_._2).getOrElse(0)
  def lastLevelOption = blocks.map(_._2).lastOption
  def level = lastLevelOption.getOrElse(0)
  def levels = blocks.map(_._2)
  def elems = blocks.map(_._1)
  def add(other: LeveledBlocks) = LeveledBlocks(this.blocks ++ other.blocks)
  def resetLevel(f: Int => Int) = {
    val breakAtFirstReset = blocks.span(b => !isReset(b)) 
    LeveledBlocks(breakAtFirstReset._1).mapLevel(f) add 
    LeveledBlocks(breakAtFirstReset._2) 
  }
  private def mapLevel(f: Int => Int) = LeveledBlocks(blocks.map((b: (Block, Int)) => (b._1, f(b._2))))
  def increment(n: Int = 1) = mapLevel(_ + n)
  def decrement(n: Int = 1) = mapLevel(_ - n)
  private val isReset = (b: (Block, Int)) => b match { case (BlockReset(), _) => true; case _ => false }
  override def equals(a: Any) = {
    a match {
      case l: LeveledBlocks => normalizeResets.blocks.equals(l.normalizeResets.blocks)
      case _ => false
    }
  }
  private def normalizeResets = {
    new LeveledBlocks(blocks.map {
      case (BlockReset(), _) => (BlockReset(), 0) 
      case other             => other 
    })
  }
}
case object LeveledBlocks {
  def apply(b: Block) = new LeveledBlocks(List((b, 0)))
  implicit object LeveledBlocksMonoid extends Monoid[LeveledBlocks] {
    def append(b1: LeveledBlocks, b2: =>LeveledBlocks) =
      (b1.lastOption, b2.headOption) match {
        case (None, _)                        => b2
        case (Some(BlockReset()), _)          => b1 add b2.resetLevel(_ - b2.firstLevel)
        case (_, Some(BlockReset()))          => b1 add b2.resetLevel(_ - b2.firstLevel)
        case (Some(BlockIndent(n)), _)         => b1 add b2.resetLevel(b1.level + _ + n)
        case (Some(BlockUnindent(n)), _)       => b1 add b2.resetLevel(b1.level + _ - n)
        case _                                => b1 add b2.resetLevel(b1.level + _)
      }
    val zero = new LeveledBlocks()
  }
  
  def foldAll[T](fs: Seq[T])(implicit convert: T => LeveledBlocks) = {
    fs.foldMap(convert)
  }

  implicit def toBlock(f: ExecutedFragment): Block = f match {
    case ExecutedResult(_, _)       => BlockTerminal() 
    case ExecutedText(_)            => BlockIndent()   
    case ExecutedPar()              => BlockUnindent()   
    case ExecutedTab(n)              => BlockIndent(n)   
    case ExecutedBacktab(n)          => BlockUnindent(n) 
    case ExecutedSpecStart(_, _, _) => BlockReset()    
    case ExecutedSpecEnd(_)         => BlockReset()    
    case ExecutedEnd()              => BlockReset()    
    case _                          => BlockNeutral()  
  } 
  implicit def toBlocks(f: ExecutedFragment): LeveledBlocks = LeveledBlocks(toBlock(f))
  implicit def toBlock(f: Fragment): Block = f match {
    case Example(_, _)   => BlockTerminal()     
    case Par()              => BlockUnindent()   
    case Tab(n)           => BlockIndent(n)       
    case Backtab(n)       => BlockUnindent(n)   
    case Text(_)         => BlockIndent()       
    case SpecStart(_, _) => BlockReset()     
    case SpecEnd(_)      => BlockReset()        
    case End()           => BlockReset()        
    case _               => BlockNeutral()        
  }
  implicit def toBlocks(f: Fragment): LeveledBlocks = LeveledBlocks(toBlock(f))
}
