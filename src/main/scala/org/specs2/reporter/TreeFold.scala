package org.specs2
package reporter

import scalaz._
import Scalaz._
import io._
import main.Arguments
import specification._
import FragmentsShow._
import LevelsFold._

/**
 * A Tree Fold is a FragmentFold which takes a seq of Fragments and transforms it to
 * a Tree of objects of type S.
 * 
 * It uses the Levels fold and an optFold function to create the elements of type S to 
 * include in the final tree. If the optFold function returns none, there will be no 
 * element for the current fragment. This is used in the JUnitDescriptionFold to 
 * avoid any other element than Text, Examples and Steps to be added to the Description
 * tree.
 * 
 * scalaz.TreeLoc is used to build a Tree of nodes S in an immutable way
 *
 */
private[specs2]
trait TreeFold[S] extends FragmentFold {
  /**
   * This method can be used to define the type of element to build from the current
   * accumulated tree and the current Fragment
   * 
   * @return an Option[S] determining if the value of type S must be including in the resulting
   * tree or not
   */
  def optFold: (T, Fragment) => Option[S]
  /** root element for the resulting tree*/
  def root: S
  
  /** 
   * Accumulator type, it is the Tree in construction:
   *  * a TreeLoc, to accumulate tree nodes
   *  * a Level, to update the current level
   */
  type T = AccumulatedTree[S]
  val initial = new AccumulatedTree(leaf(root).loc, LevelsFold.initial)

  /** Accumulated Tree */
  case class AccumulatedTree[S](private val treeLoc: TreeLoc[S], private val level: Level) {
    /** @return the whole build Tree[S] */
	  def rootTree = treeLoc.toTree
    /** @return the root element */
	  def rootLabel = rootTree.rootLabel
	  /** @return a Tree[S] at the current location */
    def tree = treeLoc.tree
    /** @return the current element */
    def label = tree.rootLabel
  }
  /** @return a fold function building an AccumulatedTree */
  def fold(implicit arguments: Arguments) = (t: T, fragment: Fragment) => {
    val AccumulatedTree(treeLocation, level) = t
    val newLevel = LevelsFold.fold(arguments)(level, fragment)
    val newTreeLoc: TreeLoc[S] = optFold(t, fragment) map { s =>
      fragment match {
        case SpecStart(_, _) => leaf(s).loc
        case other => updateTreeLoc(treeLocation, level, newLevel , leaf(s))
      }
    } getOrElse treeLocation
    
    new AccumulatedTree(newTreeLoc, newLevel)	
  }

  /**
   * Update the current tree with:
   * * the current level
   * * the next level
   * * the element to insert (a leaf actually)
   * 
   * @return the updated tree
   */
  private def updateTreeLoc(treeLoc: TreeLoc[S], level: Level, newLevel: Level, s: Tree[S]): TreeLoc[S] = {
	  level.direction match {
      case Up => { 
        if (level.level == 0 )
          treeLoc.root.insertDownLast(s)
        else if (level.lastNode != Terminal)
          treeLoc.insertDownFirst(s)
        else
          treeLoc.parent.getOrElse(treeLoc).insertDownLast(s)
      }
      case Down => {
     	  if (level.level == newLevel.level && level.lastNode == Terminal && newLevel.lastNode != Indent)
     	 	  treeLoc.parent.getOrElse(treeLoc).insertDownLast(s)
     	  else if (level.lastNode == Terminal)
     	 	  treeLoc.parent.getOrElse(treeLoc).parent.getOrElse(treeLoc).insertDownLast(s)
     	  else
     	 	  treeLoc.insertDownLast(s)
      }
	  }
  }
  
  /** Unapply method to extract a tree from an AccumulatedTree */
  object Tree {
    def unapply(acc: AccumulatedTree[S]): Option[(Tree[S])] = Some(acc.rootTree)
  }
}

private[specs2]
object TreeFold {
  def bottomUp[A, B](t: Tree[A], f: ((A, Stream[B]) => B)): Tree[B] = {
    val tbs = t.subForest.map(t => bottomUp(t, f))
    node(f(t.rootLabel, tbs.map(_.rootLabel)), tbs)
  }
}

/**
 * A Fragments tree returns a tree of Fragments from a seq of fragments
 * according to their level, computed by the Level Fold
 */
private[specs2]
trait FragmentsTree extends TreeFold[Fragment] {
  def optFold = (t: T, f: Fragment) => Some(f)
  def root = SpecStart("")
}
private[specs2]
object FragmentsTree extends FragmentsTree

