package org.specs2
package reporter
import specification._
import Fragments._
/**
 * The Selection trait implements the logic for selecting the fragments to execute
 * and sorting them according to their dependencies 
 *
 */
trait Selection {
  val select = (fragments: Fragments) => {
    extractSteps(fragments.fragments)
  }
  
  private def extractSteps(fragments: List[Fragment]): List[List[Fragment]] = {
    fragments.foldLeft(Nil: List[List[Fragment]]) { (res, cur) =>
      res match {
        case Nil => List(List(cur))
        case last :: rest => cur match {
          case Step(_) if last.exists(isExample) => List(cur) :: last :: rest 
          case Example(_, _) if last.exists(isStep) => List(cur) :: last :: rest 
          case _ => (last :+ cur) :: rest 
        }
      }
    }.reverse
  }
}
