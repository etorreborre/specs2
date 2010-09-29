package org.specs2
package reporter
import io._
import specification._
import scalaz._
import Scalaz._
import FragmentsShow._

trait SpecificationTree[S] extends Reporter with ConsoleOutput {
  import NestedLevels._
  def map: Fragment => Option[S]
  def root: S
  
  type T = (TreeLoc[S], Level)
  val initial = rootTree
  def rootTree = (leaf(root).loc, Level())

  val folder = (t: T, fragment: Fragment) => {
    val (treeLoc, level) = t
    val newLevel = updateLevel((level, fragment))
    val newTreeLoc: TreeLoc[S] =     
    fragment match {
      case SpecStart(_) => map(fragment).map(leaf(_).loc).getOrElse(treeLoc)
      case other => map(fragment).map(updateTreeLoc(level, newLevel, treeLoc, _)).getOrElse(treeLoc)
    }
    (newTreeLoc, newLevel)	
  }

  def toTree(name: String, fragments: Fragments): Tree[S] = toTree(name, fragments.fragments)
  def toTree(name: String, fragments: List[Fragment]): Tree[S] = report(SpecStart(name) :: fragments)._1.toTree
  
  private def updateTreeLoc(level: Level, newLevel: Level, treeLoc: TreeLoc[S], f: S): TreeLoc[S] = {
	level.state match {
      case Down => {
   	    if (level.level == newLevel.level && level.lastNode == Ex && newLevel.lastNode != Txt)
   	   	  treeLoc.parent.getOrElse(treeLoc).insertDownLast(leaf(f))
   	    else if (level.lastNode == Ex)
   	   	  treeLoc.parent.getOrElse(treeLoc).parent.getOrElse(treeLoc).insertDownLast(leaf(f))
   	    else
   	   	  treeLoc.insertDownLast(leaf(f))
      }
      case Up => 
        if (level.lastNode != Ex)
      	  treeLoc.insertDownFirst(leaf(f))
        else
      	  treeLoc.parent.getOrElse(treeLoc).insertDownLast(leaf(f))
	}
  }
}
object SpecificationTree extends SpecificationTree[Fragment] {
  def map: Function[Fragment, Option[Fragment]] = (f: Fragment) => Some(f)
  def root = SpecStart("")
}

