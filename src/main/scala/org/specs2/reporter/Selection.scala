package org.specs2
package reporter

import main.Arguments
import specification._
import Fragments._
/**
 * The Selection trait implements the logic for selecting the fragments to execute
 * and sorting them according to their dependencies 
 *
 */
private[specs2]
trait Selection {
  val select = (fragments: Fragments) => {
    sort(fragments)
  }
  
  private def sort(fragments: Fragments): Seq[Seq[Fragment]] = {
    fragments.fragments.view.filter(filter(fragments.arguments)).foldLeft(Nil: List[List[Fragment]]) { (res, cur) =>
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
  
  private def filter(arguments: Arguments) = (f: Fragment) => {
    f match {
      case e: Example => e.matches(arguments.ex)
      case _ => true
    }
  }
}
