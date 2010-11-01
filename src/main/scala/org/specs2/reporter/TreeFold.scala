package org.specs2
package reporter

import scalaz._
import Scalaz._
import io._
import specification._
import FragmentsShow._

private[specs2]
trait TreeFold[S] extends Fold {
  import LevelsFold._
  def optFold: (T, Fragment) => Option[S]
  def root: S
  
  case class AccumulatedTree[S](private val treeLoc: TreeLoc[S], private val level: Level) {
	  def rootTree = treeLoc.toTree
	  def rootLabel = rootTree.rootLabel
    def tree = treeLoc.tree
    def label = tree.rootLabel
  }
  object AccumulatedTree {
	  def rootTree = new AccumulatedTree(leaf(root).loc, Level())
  }
  object Tree {
	  def unapply(acc: AccumulatedTree[S]): Option[(Tree[S])] = Some(acc.rootTree)
  }
  type T = AccumulatedTree[S]
  val initial = AccumulatedTree.rootTree

  val fold = (t: T, fragment: Fragment) => {
    val AccumulatedTree(treeLocation, l) = t
    val newLevel = LevelsFold.fold(l, fragment)
    val newTreeLoc: TreeLoc[S] =     
      fragment match {
        case SpecStart(_) => optFold(t, fragment).map(leaf(_).loc).getOrElse(treeLocation)
        case other => optFold(t, fragment).map(updateTreeLoc(l, newLevel, treeLocation, _)).getOrElse(treeLocation)
      }
    new AccumulatedTree(newTreeLoc, newLevel)	
  }

  def toTree(name: String, fragments: Fragments): Tree[S] = toTree(name, fragments.fragments)
  def toTree(name: String, fragments: List[Fragment]): Tree[S] = fold((SpecStart(name) :: fragments):_*).rootTree
  private def updateTreeLoc(level: Level, newLevel: Level, treeLoc: TreeLoc[S], f: S): TreeLoc[S] = {
	  level.state match {
      case Up => { 
        if (level.level == 0 )
          treeLoc.root.insertDownLast(leaf(f))
        else if (level.lastNode != Ex)
          treeLoc.insertDownFirst(leaf(f))
        else
          treeLoc.parent.getOrElse(treeLoc).insertDownLast(leaf(f))
      }
      case Down => {
     	  if (level.level == newLevel.level && level.lastNode == Ex && newLevel.lastNode != Txt)
     	 	  treeLoc.parent.getOrElse(treeLoc).insertDownLast(leaf(f))
     	  else if (level.lastNode == Ex)
     	 	  treeLoc.parent.getOrElse(treeLoc).parent.getOrElse(treeLoc).insertDownLast(leaf(f))
     	  else
     	 	  treeLoc.insertDownLast(leaf(f))
      }
	  }
  }
}

private[specs2]
object TreeFold {
  def bottomUp[A, B](t: Tree[A], f: ((A, Stream[B]) => B)): Tree[B] = {
    val tbs = t.subForest.map(t => bottomUp(t, f))
    node(f(t.rootLabel, tbs.map(_.rootLabel)), tbs)
  }
}

private[specs2]
object FragmentsTree extends TreeFold[Fragment] {
  def optFold = (t: FragmentsTree.T, f: Fragment) => Some(f)
  def root = SpecStart("")
}

