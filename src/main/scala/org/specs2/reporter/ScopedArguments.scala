package org.specs2
package reporter

import main.Arguments
import specification._

private[specs2]
trait ScopedArguments extends ExecutedFragmentFold {
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
object ScopedArguments extends ScopedArguments