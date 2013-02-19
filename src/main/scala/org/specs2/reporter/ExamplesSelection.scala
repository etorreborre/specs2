package org.specs2
package reporter

import main.Arguments
import specification.{Example, SpecName, Fragment}

/**
 * This trait filters examples based on their description
 */
trait ExamplesSelection {
  /**
   * the filter method filters examples based on their description,
   * keeping only the ones matching the ex attribute of the arguments object
   */
  protected def filterExamples(implicit commandLineArgs: Arguments) = (fan: Seq[(Fragment, Arguments, SpecName)]) => {
    fan filter {
      case (e @ Example(_, _), args, n) => e.matches(args.overrideWith(commandLineArgs).ex)
      case (f, args, n)                 => true
    } collect { case (f, a, n) => f }
  }


}
