package org.specs2
package reporter

import main.Arguments
import specification._
import Fragments._
/**
 * The Selection trait implements the logic for filtering the fragments to execute
 * and sorting them according to their dependencies 
 *
 */
private[specs2]
trait Selection {
  /** select function returning a filtered and ordered seq of seq of Fragments */
  def select(implicit arguments: Arguments): Fragments => Seq[Seq[Fragment]]
}

/**
 * The DefaultSelection trait:
 *  * filter the fragments to execute by filtering Examples according to their names
 *  * sorts the Fragments by making sure Steps will be executed before Examples
 */
trait DefaultSelection {
  /** select function returning a filtered and ordered seq of seq of Fragments */
  def select(implicit arguments: Arguments) = (fragments: Fragments) => {
    sort(fragments.fragments.view.filter(filter))
  }
  
  /** 
   * the filter method filters examples based on their description,
   * keeping only the ones matching the ex attribute of the arguments object
   */
  protected def filter(implicit arguments: Arguments) = (f: Fragment) => {
    f match {
      case e: Example => e.matches(arguments.ex)
      case _ => true
    }
  }

  /** 
   * the sort returns sequences of fragments which can be executed concurrently.
   * Among the constraints to respect, any Step Fragment must be executed before any 
   * following Example Fragment, so that "first" and "last" actions are executed properly
   * in a Specification 
   */
  protected def sort(fragments: Seq[Fragment])(implicit arguments: Arguments): Seq[Seq[Fragment]] = {
    fragments.foldLeft(Nil: List[List[Fragment]]) { (res, cur) =>
      res match {
        case Nil => List(List(cur))
        case last :: rest => cur match {
          case Step(_) if last.exists(isExample) => List(cur) :: last :: rest 
          case Example(_, _) if last.exists(isStep) => List(cur) :: last :: rest 
          case _ => (last :+ cur) +: rest 
        }
      }
    }.reverse
  }
  
}
