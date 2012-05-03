package org.specs2
package reporter

import org.specs2.internal.scalaz._
import Scalaz._
import specification._
import data.Trees._
import StandardFragments._

/**
 * This class computes the 'level' of a given fragment. It is used to indent Fragments in
 * a ConsoleReporter and to create a tree or Descriptions in the JUnit runner
 * 
 * It does so by considering that some fragments have an effect on the indentation of the next fragment
 * 
 * - a Text fragment is an Indent(1) so it indents the next fragment once
 * - a Tab(n) fragment is an Indent(n) so it indents the next fragment n times
 * - a Backtab(n) fragment is an Unindent(n) so it un-indents the next fragment n times
 * - an Example fragment is a Terminal(), it doesn't do anything. This means that 2 consecutive examples will stay at the same level
 * - an End fragment is a Reset, it set the level of the next fragment to 0 level
 *
 */
private[specs2]
case class Levels[T](private val levelsSeq: Vector[Level[T]] = Vector[Level[T]]()) {
  /** @return true if there are no levels */
  lazy val isEmpty = levelsSeq.isEmpty
  /** @return alias for the last level */
  lazy val level = levels.lastOption.map(_.level).getOrElse(0)

  /** @return all the levels, computed with the LevelMonoid */
  lazy val levels = {
    import NestedBlocks._
    def toNestedBlock(bl: Level[T]): SpecBlock[Level[T]] = bl match {
      case b @ Level(SpecStart(_,_,_), l)           => BlockStart(bl)
      case b @ Level(ExecutedSpecStart(_,_,_), l)   => BlockStart(bl)
      case b @ Level(SpecEnd(_), l)                 => BlockEnd(bl)
      case b @ Level(ExecutedSpecEnd(_,_,_), l)     => BlockEnd(bl)
      case b                                        => BlockBit(bl)
    }
    sumContext(levelsSeq.map(toNestedBlock))(Levels.LevelMonoid[T])
  }

  /** @return the concatenation of 2 levels */
  def add(other: Levels[T]) = Levels(levelsSeq ++ other.levelsSeq)

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
    val initial = m(levels.head.t, Seq(), 0).get
    levels.drop(1).foldLeft(leaf(initial).loc) { (treeLoc, cur) =>
      val Level(block, level) = cur
      val parent = treeLoc.parentLocs.drop(level).headOption.getOrElse(treeLoc)
      m(block, parent.path.reverse.toSeq, treeLoc.size) match {
        case Some(s) => parent.insertDownLast(leaf(s))
        case None    => treeLoc
      }
    }
  }

  override def equals(a: Any) = {
    a match {
      case l: Levels[_] => levelsSeq.map(_.t).equals(l.levelsSeq.map(_.t))
      case _ => false
    }
  }
}
private[specs2]
case object Levels {
  /** @return a new Levels object for one Block */
  def apply[T](b: Level[T]) = new Levels(Vector(b))
  /** Semigroup for Level[T] */
  implicit def LevelMonoid[T]: Monoid[Level[T]] = new Monoid[Level[T]] {
    def append(l1: Level[T], l2: =>Level[T]) = {
      (l1, l2) match {
        case (LevelZero(),    _) => l2
        case (_,    LevelZero()) => l1
        case (Indent(_, n),   _) => l2.setLevel(l1.lv + n)
        case (Unindent(_, n), _) => l2.setLevel(l1.lv - n)
        case (Reset(_),       _) => l2.reset
        case (_,              _) => l2.setLevel(l1.lv)
      }
    }
    val zero: Level[T] = LevelZero[T]()
  }
  /** monoid for Levels, doing a simple aggregation */
  implicit def LevelsConcatMonoid[T] = new Monoid[Levels[T]] {
    def append(b1: Levels[T], b2: =>Levels[T]) = b1 add b2
    val zero = new Levels[T]()
  }
  /** fold a list of T to a Levels object */
  def foldAll[T](fs: Seq[T])(implicit reducer: Reducer[T, Levels[T]]) = fs.foldMap(reducer.unit)

  implicit object LevelsReducer extends Reducer[ExecutedFragment, Levels[ExecutedFragment]] {
    implicit override def unit(f: ExecutedFragment): Levels[ExecutedFragment] = Levels(LevelReducer.toLevel(f))
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
      case t @ SpecStart(_,_,_)      => Neutral(t)
      case t @ SpecEnd(_)            => Neutral(t)
      case t @ End()                 => Reset(t)        
      case t                         => Neutral(t)        
    }
    implicit override def unit(f: Fragment): Levels[Fragment] = Levels(toLevel(f))
  }
}

private[specs2]
abstract class Level[+T](val fragment: Option[T]) {

  lazy val t: T = fragment.get
  val lv: Int = 0
  type L <: Level[T]
  def level = math.max(lv, 0)
  def setLevel(lv: Int): L
  def reset: L = setLevel(0)
}

private[specs2]
case class Terminal[T](value: T) extends Level(Some(value)) {
  type L = Terminal[T]
  def setLevel(l: Int) = new Terminal(value) { override val lv = l }
}
private[specs2]
case class Indent[T](value: T, n: Int = 1) extends Level(Some(value)) {
  type L = Indent[T]
  def setLevel(l: Int) = new Indent(value, n) { override val lv = l }
}
private[specs2]
case class Unindent[T](value: T, n: Int = 1) extends Level(Some(value)) {
  type L = Unindent[T]
  def setLevel(l: Int) = new Unindent(value, n) { override val lv = l }
}
private[specs2]
case class Reset[T](value: T) extends Level(Some(value)) {
  type L = Reset[T]
  def setLevel(l: Int) = new Reset(value) { override val lv = l }
}
private[specs2]
case class Neutral[T](value: T) extends Level(Some(value)) {
  type L = Neutral[T]
  def setLevel(l: Int) = new Neutral(value) { override val lv = l }
}
private[specs2]
case class LevelZero[T]() extends Level[T](None) {
  type L = LevelZero[T]
  def setLevel(l: Int) = new LevelZero[T]() { override val lv = l }
}

private[specs2]
object Level {
  def unapply[T](l: Level[T]): Option[(T, Int)] = Some((l.t, l.lv))
}
