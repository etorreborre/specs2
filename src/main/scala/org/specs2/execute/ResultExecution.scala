package org.specs2
package execute

/**
 * This trait executes a Result and returns an approriate value when a specs2 exception is thrown
 */
trait ResultExecution { outer =>
  /** this implicit allows the execution of a Result with an `execute` method */
  implicit def resultIsExecutable(r: =>Result) = new ExecutableResult(r)
  class ExecutableResult(r: =>Result) {
    def execute = outer.execute(r)
  }
  /** execute a Result and return a Result even if there are specs2 exceptions */
  def execute(result: =>Result) =
    try {
      result
    } catch {
      case FailureException(f) => f
      case SkipException(f)    => f
      case e: Exception        => Error(e)
      case other               => throw other
    }
}
object ResultExecution extends ResultExecution