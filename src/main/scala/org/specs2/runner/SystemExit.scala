package org.specs2
package runner

import specification.ExecutedSpecification

/**
 * This trait is responsible for exiting the system after a run, depending on the success of the execution
 */
trait SystemExit {

  /**
   * exit the system for one executed specification (None means that the specification could not be executed)
   */
  def exitSystem(executed: Option[ExecutedSpecification]) {
    exitTheSystem(Seq(executed))
  }

  /**
   * exit the system with a specific code:
   *
   * - 0 if all the specifications are successful
   * - 1 if there are only failures
   * - 100 if there is any error
   * - 101 if one of them could not even be executed (this is represented by None)
   */
  def exitSystem(executed: Seq[ExecutedSpecification]) {
    exitTheSystem(executed.map(e => Some(e)))
  }

  protected def exitTheSystem(executed: Seq[Option[ExecutedSpecification]]) {
    executed.collect {
      case None                                                           => exitWith(101)
      case Some(executedSpecification) if executedSpecification.hasErrors => exitWith(100)
      case Some(executedSpecification) if executedSpecification.hasIssues => exitWith(1)
    }
    exitWith(0)
  }

  /**
   * this method can be overriden for testing
   */
  protected def exitWith(status: Int) = System.exit(status)

}
