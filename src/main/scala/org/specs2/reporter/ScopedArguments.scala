package org.specs2
package reporter

import control.Fold
import main.Arguments
import specification._

/**
 * The ScopedArguments trait allows to fold a list of Fragments and keep the 
 * arguments held by the last SpecStart fragment.
 * 
 * I suspect that there is a smarter "type-class" way to reorganize Folds so that it
 * can use any convertible type as the "thing" to fold.
 * 
 * This way the redundancy between ScopedArguments and ExecutedScopedArguments could be 
 * avoided
 */
private[specs2]
trait ScopedArguments extends FragmentFold {
  type T = Arguments
  def initial = Arguments()
  
  def fold(implicit arguments: Arguments) = { (args: Arguments, f: Fragment) => 
    f match {
      case SpecStart(_, currentArgs) => arguments.overrideWith(currentArgs)
      case _ => args
    }
  }
}
private[specs2]
object ScopedArguments extends ScopedArguments

/**
 * The ExecutedScopedArguments trait allows to fold a list of ExecutedFragments and keep the 
 * arguments held by the last ExecutedSpecStart fragment
 */
private[specs2]
trait ExecutedScopedArguments extends ExecutedFragmentFold {
  type T = Arguments
  def initial = Arguments()
  
  def fold(implicit arguments: Arguments) = { (args: Arguments, f: ExecutedFragment) => 
    f match {
      case ExecutedSpecStart(_, _, currentArgs) => arguments.overrideWith(currentArgs)
      case _ => args
    }
  }
}
private[specs2]
object ExecutedScopedArguments extends ExecutedScopedArguments