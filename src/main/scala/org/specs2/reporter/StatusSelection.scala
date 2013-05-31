package org.specs2
package reporter

import main.Arguments
import specification.{Example, SpecName, Fragment}

/**
 * This trait selects examples based on their previous execution
 */
trait StatusSelection extends WithDefaultStatisticsRepository{
  /**
   * @return fragments according to their previous execution state
   */
  def filterPrevious(implicit commandLineArgs: Arguments) = (fan: Seq[(Fragment, Arguments, SpecName)]) => {
    if (commandLineArgs.store.never) fan
    else
      fan filter {
        case (e: Example, args, specName) => {
          val currentArgs = args.overrideWith(commandLineArgs)
          !currentArgs.wasIsDefined || includePrevious(specName, e, currentArgs)
        }
        case other => true
      }
  }

  protected def includePrevious(specName: SpecName, e: Example, args: Arguments) =
    args.was(repository.previousResult(specName, e).map(_.status).getOrElse(""))

}
