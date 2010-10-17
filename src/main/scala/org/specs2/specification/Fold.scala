package org.specs2
package specification
import control.Exceptions._
import control.Throwablex._
/**
 * A Fold transforms a list of fragments to a given type T, starting from an initial value.
 * It is either used to:
 * * map each fragment to something else 
 * * to accumulate values
 * 
 * Different folds can be defined to process and report a specification. 
 * For example:
 * 
 * * a NestedLevels fold returns the "level" corresponding to a given Fragment (@see NestedLevels for more details)
 * * a FragmentsTree fold will take a list of fragments and return a tree of fragments according to
 *   their levels computed by the NestedLevels fold, optionally each Fragment node can be mapped to another object
 * * a JUnitDescription fold takes a list of fragments and returns a tree of Description objects corresponding to
 *   each Fragment  
 * 
 */
trait Fold {
  type T
  def initial: T
  val fold: Function2[T, Fragment, T]
  
  def fold(fragments: Fragments): T = trye {
    fragments.fragments.foldLeft(initial)(fold)
  } ((e: Exception) => e.printStackTrace) match {
    case Right(e) => e
    case Left(e) => initial
  }
  def handleException(e: Exception) = {
    Console.println("There was an exception during the building of fragments: " + e)
    e.getFullStackTrace.foreach(println(_))
  }
  def fold(fragments: Fragment*): T = fold(new Fragments(() => fragments.toList))
}
