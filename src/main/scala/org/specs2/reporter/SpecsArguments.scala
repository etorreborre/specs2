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
case class SpecsArguments(start: ArgumentsStart = NoStartOfArguments(), args: Arguments = Arguments()) 
case object SpecsArguments {
  implicit object SpecsArgumentsMonoid extends Monoid[SpecsArguments] {
    def append(a1: SpecsArguments, a2: =>SpecsArguments) = (a1.start, a2.start) match {
      case (_, StartOfArguments(args)) => SpecsArguments(a2.start, args)
      case _                           => a1
    }
    val zero = new SpecsArguments()
  }
  
  implicit def toSpecsArguments(f: Fragment): SpecsArguments = f match {
    case SpecStart(_, args) => SpecsArguments(StartOfArguments(args), args)     
    case _                  => SpecsArguments(NoStartOfArguments())        
  }
  implicit def toSpecsArguments(f: ExecutedFragment): SpecsArguments = f match {
    case ExecutedSpecStart(_, _, args) => SpecsArguments(StartOfArguments(args))     
    case _                             => SpecsArguments(NoStartOfArguments())        
  }

}
sealed trait ArgumentsStart
case class StartOfArguments(args: Arguments = Arguments()) extends ArgumentsStart
case class NoStartOfArguments() extends ArgumentsStart