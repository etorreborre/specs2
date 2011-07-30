package org.specs2
package reporter

import org.specs2.internal.scalaz._
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
private[specs2]
case class Levels[T](blocks: List[(Block[T], Int)] = Nil) {
  /** @return the first block */
  private def headOption = blocks.map(_._1).headOption
  /** @return the last block */
  private def lastOption = blocks.map(_._1).lastOption
  /** @return the first level or zero */
  private def firstLevel = blocks.map(_._2).headOption.getOrElse(0)
  /** @return the last level or zero */
  private def lastLevel = blocks.map(_._2).lastOption.getOrElse(0)
  /** @return the last level block in a Levels object */
  private def lastAsLevel = Levels(blocks.lastOption.toList)
  /** @return true if there are no blocks */
  def isEmpty = blocks.isEmpty
  /** @return alias for the last level */
  def level = levels.lastOption.getOrElse(0)
  /** @return all the levels, post-processing them so that there is no negative value */
  def allLevels = {
    import NestedBlocks._
    def toNestedBlock(bl: (Block[T], Int)) = bl match {
      case (b @ Block(SpecStart(_,_,_,_)), l)       => BlockStart(Levels(List(bl)))
      case (b @ Block(ExecutedSpecStart(_,_,_)), l) => BlockStart(Levels(List(bl)))
      case (b @ Block(SpecEnd(_)), l)               => BlockEnd(Levels(List(bl)))
      case (b @ Block(ExecutedSpecEnd(_,_,_)), l)   => BlockEnd(Levels(List(bl)))
      case (b, l)                                   => BlockBit(Levels(List(bl)))
    }
    import Levels._
    val summed = sumContext(blocks.map(toNestedBlock), (l: Levels[T]) => l.lastAsLevel)(LevelsMonoid[T])
    implicit val m = LevelsConcatMonoid[T]
    val all = summed.foldLeft(m.zero)(m append (_, _)).blocks
    all map { case (b, l) => if (l < 0) (b, 0) else (b, l) }
  }
  def levels = allLevels.map(_._2)

  /** @return the concatenation of 2 levels */
  def add(other: Levels[T]) = Levels(this.blocks ++ other.blocks)
  /** 
   * @return reset the levels of all blocks by incrementing or decrementing the level 
   *         value, until a Reset block is met
   */
  def resetLevel(f: Int => Int) = {
    val setLevel = (b: Block[T], l: Int) => (b, f(l))
    val breakAtFirstReset = blocks.span(b => !isReset(b))
    Levels(breakAtFirstReset._1).mapLevel(setLevel) add
    Levels(breakAtFirstReset._2)
  }

  /** =
   * @return reset all the levels of all blocks by incrementing or decrementing the level 
   *         value
   */
  private def mapLevel(f: (Block[T], Int) => (Block[T], Int)) = Levels(blocks.map(f.tupled))

  /**
   * @return a Tree[T] based on the level of each block
   */
  def toTree: Tree[T] = toTreeLoc.toTree

  /**
   * map each node to another type given: the current type, the path from root (without the current node), the node number
   *
   * @return a Tree[S] based on the level of each block, mapping each node to value of type
   *         S and possibly skipping nodes
   */
  def toTree[S](m: (T, Seq[S], Int) => Option[S]): Tree[S] = toTreeLoc(m).toTree
  /**
   * map each node to another type given: the current type, the node number
   *
   * @return a Tree[S] based on the level of each block, mapping each node to value of type
   *         S and possibly skipping nodes
   */
  def toTree[S](m: (T, Int) => Option[S]): Tree[S] = {
    def m1(t: T, s: Seq[S], i: Int) = m(t, i)
    toTree[S](m1 _)
  }

  /**
   * @return a TreeLoc[T] based on the level of each block
   */
  def toTreeLoc: TreeLoc[T] = toTreeLoc((t:T, parentsPath: Seq[T], i: Int) => Some(t))
  /**
   * WARNING this method assumes that the Levels are not empty!!
   * 
   * @return a Tree[S] based on the level of each block, mapping each node to value of type
   *         S and possibly skipping nodes, passing the numeric label of the current node. 
   * @see JUnitDescriptions
   */
  def toTreeLoc[S](m: (T, Seq[S], Int) => Option[S]): TreeLoc[S] = {
    val all = allLevels
    val initial = m(all.head._1.t, Seq(), 0).get
    all.drop(1).foldLeft(leaf(initial).loc) { (treeLoc, cur) =>
      val (block, level) = cur
      val parent = treeLoc.parentLocs.drop(level).headOption.getOrElse(treeLoc)
      m(block.t, parent.path.reverse.toSeq, treeLoc.size) match {
        case Some(s) => parent.insertDownLast(leaf(s))
        case None    => treeLoc
      }
    }
  } 
  private val isReset = (b: (Block[T], Int)) => b._1 match {
    case BlockReset(t) => true
    case other         => false
  }

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
      case (BlockReset(t), _)      => (BlockReset(t), 0)
      case other                   => other
    })
  }
}
private[specs2]
case object Levels {
  /** @return a new Levels object for one Block */
  def apply[T](b: Block[T]) = new Levels(List((b, 0)))
  /** monoid for Levels */
  def LevelsMonoid[T] = new Monoid[Levels[T]] {
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
  /** monoid for Levels, doing a simple aggregation */
  implicit def LevelsConcatMonoid[T] = new Monoid[Levels[T]] {
    def append(b1: Levels[T], b2: =>Levels[T]) = b1 add b2
    val zero = new Levels[T]()
  }
  /** fold a list of T to a Levels object */
  def foldAll[T](fs: Seq[T])(implicit reducer: Reducer[T, Levels[T]]) = {
    fs.foldMap(reducer.unit)
  }
  implicit object LevelsReducer extends Reducer[ExecutedFragment, Levels[ExecutedFragment]] {
    implicit def toBlock(f: ExecutedFragment): Block[ExecutedFragment] = f match {
      case t @ ExecutedResult(_,_,_,_,_)     => BlockTerminal(t)
      case t @ ExecutedText(_, _)            => BlockIndent(t)
      case t @ ExecutedTab(n, _)             => BlockIndent(t, n)
      case t @ ExecutedBacktab(n, _)         => BlockUnindent(t, n)
      case t @ ExecutedSpecStart(_,_,_)      => BlockNeutral(t)
      case t @ ExecutedSpecEnd(_,_,_)        => BlockNeutral(t)
      case t @ ExecutedEnd( _)               => BlockReset(t)
      case t                                 => BlockNeutral(t)
    } 
    implicit override def unit(f: ExecutedFragment): Levels[ExecutedFragment] = Levels[ExecutedFragment](toBlock(f))
    
  }
  implicit object FragmentLevelsReducer extends Reducer[Fragment, Levels[Fragment]] {
    implicit def toBlock(f: Fragment): Block[Fragment] = f match {
      case t @ Example(_, _)         => BlockTerminal(t)     
      case t @ Tab(n)                => BlockIndent(t, n)
      case t @ Backtab(n)            => BlockUnindent(t, n)   
      case t @ Text(_)               => BlockIndent(t)       
      case t @ SpecStart(_,_,_,_)  => BlockNeutral(t)
      case t @ SpecEnd(_)            => BlockNeutral(t)
      case t @ End()                 => BlockReset(t)        
      case t                         => BlockNeutral(t)        
    }
    implicit override def unit(f: Fragment): Levels[Fragment] = Levels(toBlock(f))
  }
}
/** this represent a fragment of a specification that needs to be indented as a block */
private[specs2]
sealed trait Block[T] {
  val t: T
}
private[specs2]
object Block {
  def unapply[T](b: Block[T]) = Some(b.t)
}
private[specs2] case class BlockTerminal[T](t: T = null) extends Block[T]
private[specs2] case class BlockIndent[T](t: T = null, n: Int = 1)  extends Block[T]
private[specs2] case class BlockUnindent[T](t: T = null, n: Int = 1) extends Block[T]
private[specs2] case class BlockReset[T](t: T = null)    extends Block[T]
private[specs2] case class BlockNeutral[T](t: T = null)  extends Block[T]
