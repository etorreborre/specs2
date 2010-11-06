package org.specs2
package reporter

import main.Arguments
import specification._

/**
 * This fold computes the 'level' of a given fragment. It is used to indent Fragments in
 * a ConsoleReporter and to create a tree or Descriptions in the JUnit runner
 * 
 * It does so by considering that:
 * 
 * * when a Text fragment follows a Text fragment we're going up one
 *   (so the second text fragment can be indented relatively to the first one)
 * * when an Example fragment follows a Text fragment we're going up one
 * * when an Example fragment follows an Example fragment we're staying on the same level
 * * when a Text fragment follows an Example fragment we're going down one level
 *
 * There are some cases though where the user would have to explicitly reset the level by
 * inserting an 'end' marker:
 * 
 * "this block"^
 *   "has 1 example" ! { true }^
 * "this other block"^
 *   "has a nested text"^
 *     "with 1 example" ! { true }^
 *     end^
 * "this third block"^
 *   "will not be nested thanks to the previous end marker" ! { true }^
 *
 */
private[specs2]
trait LevelsFold extends FragmentFold {
  import StandardFragments._

  type T = Level
  case class Level(level: Int = 0, direction: Direction = Up, lastNode: LastNode = Txt)
  lazy val initial = new Level()
  
  /**
   * This returns a function which computes the current level and the next Level object
   * storing the current state
   */
  def level(implicit arguments: Arguments): Function2[Level, Fragment, (Int, Level)] = 
    (t: T, f: Fragment) => (t, f) match {
	    case p => (currentLevel(p), fold.tupled(p))
    }
  
  /**
   * This function computes the current level for a given Fragment
   */
  val currentLevel: Function[(Level, Fragment), Int] = { 
	  case (a, f @ Text(s)) if (a.direction == Down && a.lastNode == Ex) => (a.level - 1)
	  case (a, f) => a.level
  }
  
  def fold(implicit arguments: Arguments) = (t: T, f: Fragment) => (t, f) match {
    // end resets the level
	  case (a, End()) => Level()
	  case (a, Text(s)) => {
	    val newLevel = a.direction match {
	   	  case Up => a.copy(level = a.level + 1)
	   	  case Down if (a.lastNode != Ex) => a.copy(level = a.level + 1, direction = Up)
	   	  case _ => a
	    } 
	    newLevel copy (lastNode = Txt)
	  }
	  case (a, e @ Example(s, body)) => a.copy(direction = Down, lastNode = Ex)
	  case (t, f) => t
  }
 
}
private[specs2]
object LevelsFold extends LevelsFold

private[specs2]
sealed trait Direction
case object Up extends Direction
case object Down extends Direction

private[specs2]
sealed trait LastNode
case object Ex extends LastNode
case object Txt extends LastNode

