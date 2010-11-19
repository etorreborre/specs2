package org.specs2
package reporter

import scalaz._
import Scalaz._
import main.Arguments
import specification._

/**
 * The ScopedArguments trait allows to fold a list of Fragments and keep the 
 * arguments held by the last SpecStart fragment.
 */
private[specs2]
case class SpecsArguments(arguments: List[(ArgumentsStart, Arguments)] = Nil) {
  def args = arguments.last._2
  def toList = arguments.map(_._2)
}

private[specs2]
case object SpecsArguments {
  def create(a: ArgumentsStart, args: Arguments = Arguments()) = new SpecsArguments(List((a, args)))
  
  implicit def argumentsState(fragment: ExecutedFragment) = state { (a: Arguments) => (a, fragment) match {
      case (_, f @ ExecutedSpecStart(_, _, args)) => (args, args)     
      case (args, f)                              => (args, args)        
    } 
  }
  
  implicit object SpecsArgumentsMonoid extends Monoid[SpecsArguments] {
    def append(a1: SpecsArguments, a2: =>SpecsArguments) = new SpecsArguments(a1.arguments ++ a2.arguments)
    val zero = new SpecsArguments()
  }
  implicit object FragmentSpecsArgumentsReducer extends Reducer[Fragment, SpecsArguments] {
    implicit override def unit(f: Fragment): SpecsArguments = f match {
      case SpecStart(_, args) => create(StartOfArguments(), args)     
      case _                  => create(NoStartOfArguments())        
    }
  }
  implicit object ExecutedFragmentSpecsArgumentsReducer extends Reducer[ExecutedFragment, SpecsArguments] {
    implicit override def unit(f: ExecutedFragment): SpecsArguments = f match {
      case ExecutedSpecStart(_, _, args) => create(StartOfArguments(), args)     
      case _                             => create(NoStartOfArguments())        
    }
  }

}
private[specs2]
sealed trait ArgumentsStart
case class StartOfArguments() extends ArgumentsStart
case class NoStartOfArguments() extends ArgumentsStart