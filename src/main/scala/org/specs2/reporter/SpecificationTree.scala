package org.specs2
package reporter
import specification._
import scalaz._
import Scalaz._
import FragmentsShow._

trait SpecificationTree {
  def toTree(name: String, specification: SpecificationStructure): Tree[Fragment] = {
	toTree(name, specification.examples.fragments)
  }
  def toTree(name: String, fragments: List[Fragment]): Tree[Fragment] = {
	import NestedLevels._
	
	fragments.foldLeft((treeLoc(Text(name)), Level())) { (res, fragment) => 
	  val (treeLoc, level) = res
	  val newLevel = updateLevel((level, fragment))
	  val newTreeLoc = level.state match {
  	    case Down => {
	 	  if (level.level == newLevel.level && level.lastNode == Ex && newLevel.lastNode != Txt)
	 	 	treeLoc.parent.getOrElse(treeLoc).insertDownLast(leaf(fragment))
	 	  else if (level.lastNode == Ex)
	 	 	treeLoc.parent.getOrElse(treeLoc).parent.getOrElse(treeLoc).insertDownLast(leaf(fragment))
	 	  else
	 	 	treeLoc.insertDownLast(leaf(fragment))
  	    }
	 	case Up => 
  	      if (level.lastNode != Ex)
  	    	treeLoc.insertDownFirst(leaf(fragment))
  	      else
  	    	treeLoc.parent.getOrElse(treeLoc).insertDownLast(leaf(fragment))
	  } 
      (newTreeLoc, newLevel)	
	}._1.toTree
  }
  
  private def treeLoc(fragment: Fragment) = loc(leaf(fragment), Stream.empty, Stream.empty, Stream.empty)
}

