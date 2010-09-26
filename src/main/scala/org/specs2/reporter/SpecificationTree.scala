package org.specs2
package reporter
import specification._
import scalaz._
import Scalaz._

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
  	    case Down => treeLoc.insertDownLast(leaf(fragment))
	 	case Up => treeLoc.insertRight(leaf(fragment))
	  } 
      (newTreeLoc, newLevel)	
	}._1.tree
  }
  
  private def treeLoc(fragment: Fragment) = loc(leaf(fragment), Stream.empty, Stream.empty, Stream.empty)
}