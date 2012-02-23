package org.specs2
package reporter

import org.specs2.internal.scalaz._
import Scalaz._
import scala.math._
import main.Arguments
import specification._
import data.Trees._
import StandardFragments._
import scala.collection.mutable.ArrayBuffer

/**
 * This class computes the 'level' of a given fragment. It is used to indent Fragments in
 * a ConsoleReporter and to create a tree or Descriptions in the JUnit runner
 * 
 * It does so by considering that:
 * 
 * - when a Text fragment follows a Text fragment we're going up one level
 *   (so the second text fragment can be indented relatively to the first one)
 * - when an Example fragment follows a Text fragment we're going up one level
 * - when an Example fragment follows an Example fragment we're staying on the same level
 * - when a Text fragment follows an Example fragment we're going down one level
 * - when a paragraph follows anything, we're going down one level
 * - when there is a end, we reset the levels to zero
 * - when there is a tab we indent everything following the tab (1 level is the default)
 * - when there is a backtab we unindent everything following the tab (1 level is the default)
 * 
 */
private[specs2]
case class Levels[T](levels: ArrayBuffer[Level[T]] = new ArrayBuffer[Level[T]]()) {
  /** @return the first block */
  private lazy val headOption = levels.headOption.map(_.t)
  /** @return the last block */
  private lazy val lastOption = levels.lastOption.map(_.t)
  /** @return the first level or zero */
  private lazy val firstLevel = levels.headOption.map(_.level).getOrElse(0)
  /** @return the last level or zero */
  private lazy val lastLevel = levels.lastOption.map(_.level).getOrElse(0)
  /** @return true if there are no levels */
  lazy val isEmpty = levels.isEmpty
  /** @return alias for the last level */
  lazy val level = levels.lastOption.map(_.level).getOrElse(0)

  /** @return all the levels, post-processing them so that there is no negative value */
  lazy val allLevels = {
    import NestedBlocks._
    def toNestedBlock(bl: Level[T]): SpecBlock[Level[T]] = bl match {
      case b @ Level(SpecStart(_,_,_,_), l)       => BlockStart(bl)
      case b @ Level(ExecutedSpecStart(_,_,_), l) => BlockStart(bl)
      case b @ Level(SpecEnd(_), l)               => BlockEnd(bl)
      case b @ Level(ExecutedSpecEnd(_,_,_), l)   => BlockEnd(bl)
      case b                                      => BlockBit(bl)
    }
    import Levels._
    sumContext(levels.map(toNestedBlock))(LevelSemigroup[T])
  }

  /** @return the concatenation of 2 levels */
  def add(other: Levels[T]) = Levels(this.levels ++ other.levels)
  /** 
   * @return reset the levels of all levels by incrementing or decrementing the level 
   *         value, until a Reset block is met
   */
  def resetLevel(f: Int => Int) = {

    val resettedBlocks = levels.foldLeft((true, new ArrayBuffer[Level[T]]())) { (res, cur) =>
      val (beforeResetBlock, result) = res
      val Level(t, lv) = cur
      if (beforeResetBlock && !isReset(cur)) (true, result :+ cur.setLevel(f(lv)))
      else                                   (false, result :+ cur)
    }._2
    Levels(resettedBlocks)
  }

  /**
   * @return a Tree[T] based on the level of each block
   */
  lazy val toTree: Tree[T] = toTreeLoc.toTree

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
  lazy val toTreeLoc: TreeLoc[T] = toTreeLoc((t:T, parentsPath: Seq[T], i: Int) => Some(t))
  /**
   * WARNING this method assumes that the Levels are not empty!!
   * 
   * @return a Tree[S] based on the level of each block, mapping each node to value of type
   *         S and possibly skipping nodes, passing the numeric label of the current node. 
   * @see JUnitDescriptions
   */
  def toTreeLoc[S](m: (T, Seq[S], Int) => Option[S]): TreeLoc[S] = {
    val all = allLevels
    val initial = m(all.head.t, Seq(), 0).get
    all.drop(1).foldLeft(leaf(initial).loc) { (treeLoc, cur) =>
      val Level(block, level) = cur
      val parent = treeLoc.parentLocs.drop(level).headOption.getOrElse(treeLoc)
      m(block, parent.path.reverse.toSeq, treeLoc.size) match {
        case Some(s) => parent.insertDownLast(leaf(s))
        case None    => treeLoc
      }
    }
  } 
  private val isReset = (b: Level[T]) => b match {
    case Reset(_) => true
    case other    => false
  }

  override def equals(a: Any) = {
    a match {
      case l: Levels[_] => levels.map(_.t).equals(l.levels.map(_.t))
      case _ => false
    }
  }
}
private[specs2]
case object Levels {
  /** @return a new Levels object for one Block */
  def apply[T](b: Level[T]) = new Levels(ArrayBuffer(b))
  /** monoid for Levels */
  def LevelsMonoid[T] = new Monoid[Levels[T]] {
    def append(b1: Levels[T], b2: =>Levels[T]) =
      (b1.lastOption, b2.headOption) match {
        case (None, _)                   => b2
        case (Some(Reset(t)), _)         => b1 add b2.resetLevel(_ - b2.firstLevel)
        case (_, Some(Reset(t)))         => b1 add b2.resetLevel(_ - b2.firstLevel)
        case (Some(Indent(t, n)), _)     => b1 add b2.resetLevel(b1.lastLevel + _ + n)
        case (Some(Unindent(t, n)), _)   => b1 add b2.resetLevel(b1.lastLevel + _ - n)
        case _                           => b1 add b2.resetLevel(b1.lastLevel + _)
      }

    val zero = new Levels[T]()
  }
  /** monoid for Option[Level[T]] */
  def LevelOptionMonoid[T] = new Monoid[Option[Level[T]]] {
    def append(b1: Option[Level[T]], b2: =>Option[Level[T]]) = LevelsMonoid.append(Levels(ArrayBuffer(b1.toSeq:_*)),
                                                                                   Levels(ArrayBuffer(b2.toSeq:_*))).levels.lastOption
    val zero: Option[Level[T]] = None
  }
  /** Semigroup for Level[T] */
  def LevelSemigroup[T]: Semigroup[Level[T]] = new Semigroup[Level[T]] {
    def append(l1: Level[T], l2: =>Level[T]) =
      (l1, l2) match {
        case (Reset(t),       _) => l2
        case (Indent(t, n),   Indent(t2, n2)) => l2.addLevel(l1.lv + n + n2)
        case (Indent(t, n),   Unindent(t2, n2)) => l2.addLevel(l1.lv + n - n2)
        case (Indent(t, n),   _) => l2.addLevel(l1.lv + n)
        case (Unindent(t, n), Unindent(t2, n2)) => l2.addLevel(l1.lv - n - n2)
        case (Unindent(t, n), Indent(t2, n2)) => l2.addLevel(l1.lv - n + n2)
        case (Unindent(t, n), _) => l2.addLevel(l1.lv - n)
        case (Terminal(t),  Terminal(t2)) => l2.addLevel(l1.lv + 2)
        case (Terminal(t),    _) => l2.addLevel(l1.lv + 1)
        case (_,              _) => l2.addLevel(l1.lv)
      }
  }
  implicit def LevelMonoid[T]: Monoid[Level[T]] = new Monoid[Level[T]] {
    def append(l1: Level[T], l2: =>Level[T]) = LevelSemigroup[T].append(l1, l2)
    val zero: Level[T] = LevelZero[T]()
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
    implicit override def unit(f: ExecutedFragment): Levels[ExecutedFragment] = Levels(ArrayBuffer(LevelReducer.toLevel(f)))
  }
  implicit object LevelReducer extends Reducer[ExecutedFragment, Level[ExecutedFragment]] {
    implicit def toLevel: ExecutedFragment => Level[ExecutedFragment] = (f: ExecutedFragment) => f match {
      case t @ ExecutedResult(_,_,_,_,_)     => Terminal(t)
      case t @ ExecutedText(_, _)            => Indent(t)
      case t @ ExecutedTab(n, _)             => Indent(t, n)
      case t @ ExecutedBacktab(n, _)         => Unindent(t, n)
      case t @ ExecutedSpecStart(_,_,_)      => Neutral(t)
      case t @ ExecutedSpecEnd(_,_,_)        => Neutral(t)
      case t @ ExecutedEnd( _)               => Reset(t)
      case t                                 => Neutral(t)
    } 
    implicit override def unit(f: ExecutedFragment): Level[ExecutedFragment] = toLevel(f)
    
  }
  implicit object FragmentLevelsReducer extends Reducer[Fragment, Levels[Fragment]] {
    implicit def toLevel(f: Fragment): Level[Fragment] = f match {
      case t @ Example(_, _)         => Terminal(t)     
      case t @ Tab(n)                => Indent(t, n)
      case t @ Backtab(n)            => Unindent(t, n)   
      case t @ Text(_)               => Indent(t)       
      case t @ SpecStart(_,_,_,_)    => Neutral(t)
      case t @ SpecEnd(_)            => Neutral(t)
      case t @ End()                 => Reset(t)        
      case t                         => Neutral(t)        
    }
    implicit override def unit(f: Fragment): Levels[Fragment] = Levels(toLevel(f))
  }
}

private[specs2]
abstract class Level[+T](val t: T) {
  val lv: Int = 0
  type L <: Level[T]
  def level = math.max(lv, 0)
  def setLevel(lv: Int): L
  def reset: L = setLevel(0)
  def addLevel(n: Int): L = setLevel(lv + n)
}

private[specs2]
object Level {
  def unapply[T](l: Level[T]): Option[(T, Int)] = Some((l.t, l.lv))
}
private[specs2]
case class Terminal[T](value: T) extends Level(value) {
  type L = Terminal[T]
  def setLevel(n: Int) = new Terminal(value) { override val lv = n }
}
private[specs2]
case class Indent[T](value: T, n: Int = 1) extends Level(value) {
  type L = Indent[T]
  def setLevel(n: Int) = new Indent(value, n) { override val lv = n }
}
private[specs2]
case class Unindent[T](value: T, n: Int = 1) extends Level(value) {
  type L = Unindent[T]
  def setLevel(n: Int) = new Unindent(value, n) { override val lv = n }
}
private[specs2]
case class Reset[T](value: T) extends Level(value) {
  type L = Reset[T]
  def setLevel(n: Int) = new Reset(value) { override val lv = n }
}
private[specs2]
case class Neutral[T](value: T) extends Level(value) {
  type L = Neutral[T]
  def setLevel(n: Int) = new Neutral(value) { override val lv = n }
}
private[specs2]
case class LevelZero[T]() extends Level[T](null) {
  type L = LevelZero[T]
  def setLevel(n: Int) = new LevelZero[T]() { override val lv = n }
}

