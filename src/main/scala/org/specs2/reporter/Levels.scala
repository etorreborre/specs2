package org.specs2
package reporter

import specification._

private[specs2]
trait LevelsFold extends Fold {
  import StandardFragments._

  case class Level(level: Int = 0, state: Direction = Up, lastNode: LastNode = Txt)
  type T = Level
  lazy val initial = new Level()
  
  val level: Function[(T, Fragment), (Int, T)] = {
	  case p => (currentLevel(p), fold.tupled(p))
  }
  val currentLevel: Function[(T, Fragment), Int] = { 
	  case (a, f @ Text(s)) if (a.state == Down && a.lastNode == Ex) => (a.level - 1)
	  case (a, f) => a.level
  }
  
  val fold: Function2[T, Fragment, T] = (t: T, f: Fragment) => (t, f) match {
	  case (a, End()) => Level()
	  case (a, Par()) => a
	  case (a, Br()) => a
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
	  case (a, e @ Example(s, body)) => a.copy(state = Down, lastNode = Ex)
	  case (t, f) => t
  }
 
}
private[specs2]
sealed trait Direction
case object Up extends Direction
case object Down extends Direction

private[specs2]
sealed trait LastNode
case object Ex extends LastNode
case object Txt extends LastNode

private[specs2]
object LevelsFold extends LevelsFold