package org.specs2
package runner
import specification._

trait NestedLevels {
  case class Accumulator(level: Int = 0, state: Direction = Up, lastNode: LastNode = Txt)
  type T = Accumulator
  def initial = new Accumulator()
  
  val currentLevel: PartialFunction[(T, Fragment), Int] = { 
	case (a, f @ Text(s)) if (a.state == Down && a.lastNode == Ex) => (a.level - 1)
	case (a, f) => (a.level)
	case _ => 0
  }
  
  val updateLevel: PartialFunction[(T, Fragment), T] = {
	case (a, `end`) => initial
	case (a, `par`) => a
	case (a, `br`) => a
	case (a, Text(s)) => {
	  a.state match {
	 	case Up => a.copy(level = a.level + 1, lastNode = Txt)
	 	case Down => {
  	 	   if (a.lastNode == Ex)
	 	     a.copy(lastNode = Txt)
  	 	   else 
	 	     a.copy(level = a.level + 1, lastNode = Txt, state = Up)
	 	}
	  }
	}
	case (a, e @ Example(s, Some(body))) => {
	  a.copy(state = Down, lastNode = Ex)
	}
	case (t, f) => t
  }
  
  sealed trait Direction
  case object Up extends Direction
  case object Down extends Direction
  sealed trait LastNode
  case object Ex extends LastNode
  case object Txt extends LastNode
}
object NestedLevels extends NestedLevels