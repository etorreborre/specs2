package org.specs2
package reporter

import main.Arguments
import control.LazyParameters._
import specification._
import Fragments._
/**
 * The Selection trait implements the logic for filtering the fragments to execute
 * and sorting them according to their dependencies 
 *
 */
//private[specs2]
trait Selection {
  /** select function returning a filtered and ordered seq of seq of Fragments */
  def select(implicit arguments: Arguments): Fragments => Seq[Fragments]
}

/**
 * The DefaultSelection trait:
 *  * filter the fragments to execute by filtering Examples according to their names
 *  * sorts the Fragments by making sure Steps will be executed before Examples
 */
trait DefaultSelection {
  /** select function returning a filtered and ordered seq of seq of Fragments */
  def select(implicit arguments: Arguments) = (fragments: Fragments) => {
    val args = arguments.overrideWith(fragments.arguments)
    sort(filterFold.foldAll(fragments.fragments.view)._2)(args)
  }
  private val filterFold = new FragmentFold {
    val scopedArguments: ScopedArguments = new ScopedArguments {}
    type T = (scopedArguments.T, Seq[Fragment])
    def initial = (scopedArguments.initial, Seq[Fragment]())

    def fold(implicit arguments: Arguments) = (argsAndResult: T, f: Fragment) => {
      val (args, result) = argsAndResult
      val newResult = 
        if (filter(arguments.overrideWith(args))(f)) result :+ f
        else result
      (scopedArguments.fold(arguments)(args, f), newResult)
    }
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
  protected def sort(fragments: Seq[Fragment])(implicit arguments: Arguments): Seq[Fragments] = {
    if (arguments.sequential) fragments.map(f => Fragments(f))
    else isolateSteps(fragments)(arguments)
  }
  
  protected def isolateSteps(fragments: Seq[Fragment])(implicit arguments: Arguments): Seq[Fragments] = {
    isolateFold.foldAll(fragments)._2.reverse
  }
  
  private val isolateFold = new FragmentFold {
    val scopedArguments: ScopedArguments = new ScopedArguments {}
    type T = (scopedArguments.T, Seq[Fragments])
    def initial = (scopedArguments.initial, Seq[Fragments]())

    def fold(implicit arguments: Arguments) = (argsAndResult: T, f: Fragment) => {
      val (args, result) = argsAndResult
      val overridenArgs = arguments.overrideWith(args)
      
      val newResult = result match {
        case Nil => List(Fragments(List(f))(overridenArgs))
        case last :: rest => f match {
          case Step(_) if last.fragments.exists(isExample) => Fragments(List(f))(overridenArgs) :: last :: rest 
          case Example(_, _) if last.fragments.exists(isStep) => Fragments(List(f))(overridenArgs) :: last :: rest 
          case _ => Fragments(last.fragments :+ f)(overridenArgs):: rest 
        }
      }
      (scopedArguments.fold(arguments)(args, f), newResult)
    }
  }

}
