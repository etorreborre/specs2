package org.specs2
package reporter
import io._
import specification._
import scalaz._
import Scalaz._
import FragmentsShow._

trait SpecificationTree[S] extends Reporter with ConsoleOutput {
  import NestedLevels._
  def map: Fragment => S
  
  type T = (TreeLoc[S], Level)
  val initial = emptySpecTree
  def emptySpecTree = (treeLocRoot(map(SpecStart(""))), Level())

  val folder = (t: T, fragment: Fragment) => {
    val (treeLoc, level) = t
    val newLevel = updateLevel((level, fragment))
    val newTreeLoc: TreeLoc[S] =     
      fragment match {
    	case SpecStart(_) => treeLocRoot(map(fragment))
    	case _ => {
          level.state match {
            case Down => {
   	          if (level.level == newLevel.level && level.lastNode == Ex && newLevel.lastNode != Txt)
   	         	treeLoc.parent.getOrElse(treeLoc).insertDownLast(leaf(map(fragment)))
   	          else if (level.lastNode == Ex)
   	         	treeLoc.parent.getOrElse(treeLoc).parent.getOrElse(treeLoc).insertDownLast(leaf(map(fragment)))
   	          else
   	         	treeLoc.insertDownLast(leaf(map(fragment)))
            }
            case Up => 
              if (level.lastNode != Ex)
            	treeLoc.insertDownFirst(leaf(map(fragment)))
              else
            	treeLoc.parent.getOrElse(treeLoc).insertDownLast(leaf(map(fragment)))
    	}
      }
    } 
    (newTreeLoc, newLevel)	
  }
  def toTree(name: String, fragments: Fragments): Tree[S] = toTree(name, fragments.fragments)
  def toTree(name: String, fragments: List[Fragment]): Tree[S] = report(SpecStart(name) :: fragments)._1.toTree	
  
  private def treeLocRoot[S](fragment: S) = loc(leaf(fragment), Stream.empty, Stream.empty, Stream.empty)
}
object SpecificationTree extends SpecificationTree[Fragment] {
  def map: Function[Fragment, Fragment] = (f: Fragment) => f
}

