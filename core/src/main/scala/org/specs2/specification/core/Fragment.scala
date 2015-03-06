package org.specs2
package specification
package core

import execute.Result
import scala.concurrent.duration.{FiniteDuration, Duration}
import scalaz.Show
import scalaz.syntax.show._

/**
 * Fragment of a specification
 * 
 * It has a description (generally text but sometimes not, for a step for example)
 * It has an execution which might do or don't do anything (for examples it runs some code)
 */
case class Fragment(description: Description, execution: Execution, location: Location = StacktraceLocation()) {
  /** @return the result of this fragment if it has been executed, Success otherwise */
  def executionResult = execution.result
  /** @return true if this fragment has been executed */
  def isExecuted = execution.isExecuted
  /** @return true if this fragment can be executed */
  def isExecutable = execution.isExecutable

  /** @return stop the execution of the next fragment based on a condition */
  def mustStopOn(r: Result) = execution.nextMustStopIf(r)

  /** various methods to stop the execution of the next fragment */
  def stopOn(r: Result) = updateExecution(_.stopNextIf(r))
  def stopOnError       = stopWhen(_.isError)
  def stopOnFail        = stopWhen(_.isFailure)
  def stopOnSkipped     = stopWhen(_.isSkipped)

  def stopWhen(f: Result => Boolean) = updateExecution(_.stopNextIf(f))

  /** various methods to stop the execution of the next fragment */
  def join              = updateExecution(_.join)
  def isolate           = updateExecution(_.makeGlobal)

  def makeGlobal(when: Boolean)           = updateExecution(_.makeGlobal(when))
  def setTimeout(timeout: FiniteDuration) = updateExecution(_.setTimeout(timeout))

  /** skip this fragment */
  def skip              = updateExecution(_.skip)
  def updateExecution(f: Execution => Execution) = copy(execution = f(execution))
  def updateRun(r: (Env => Result) => (Env => Result)) = updateExecution(_.updateRun(r))

  /** update the description */
  def updateDescription(f: Description => Description) = copy(description = f(description))

  /** set a different execution */
  def setExecution(e: Execution) = updateExecution(_ => e)

  /** set the previous execution result when known */
  def setPreviousResult(r: Option[Result]) = copy(execution = execution.setPreviousResult(r))
  def was(statusCheck: String => Boolean) = execution.was(statusCheck)

  def setLocation(location: Location) = copy(location = location)
  override def toString = s"Fragment($description, $execution) ($location)"
}

object Fragment {
  implicit def showInstance(implicit showd: Show[Description], showe: Show[Execution]): Show[Fragment] = new Show[Fragment] {
    override def shows(f: Fragment): String =
      s"Fragment(${f.description.shows})"
  }

  def isText(f: Fragment) = (f.description match {
    case t: Text => true
    case t: Code => true
    case _       => false
  }) && !f.isExecutable

  def isEmptyText(f: Fragment): Boolean =
    isText(f) && f.description.show.trim.isEmpty

  def isExample(f: Fragment) = (f.description match {
    case t: Text => true
    case t: Code => true
    case _       => false
  }) && f.isExecutable

  def isStepOrAction(f: Fragment) =
    (f.description == NoText) && f.isExecutable

  def isStep(f: Fragment) =
    isStepOrAction(f) && f.execution.mustJoin

  def isAction(f: Fragment) =
    isStepOrAction(f) && !f.execution.mustJoin

  def isMarker(f: Fragment) = f.description match {
    case m: Marker => true
    case _         => false
  }

  def isTab(f: Fragment) = f.description match {
    case Tab(_) => true
    case _      => false
  }

  def isBacktab(f: Fragment) = f.description match {
    case Backtab(_) => true
    case _          => false
  }

  def isSpecificationRef(f: Fragment) = f.description match {
    case l: SpecificationRef => true
    case _                   => false
  }

  def specificationRef: PartialFunction[Fragment, SpecificationRef] = {
    case Fragment(l: SpecificationRef,_,_) => l
  }

  def linkReference: PartialFunction[Fragment, SpecificationRef] = {
    case f @ Fragment(l: SpecificationRef,_,_) if f.isExecutable => l
  }

  def seeReference: PartialFunction[Fragment, SpecificationRef] = {
    case f @ Fragment(l: SpecificationRef,_,_) if !f.isExecutable => l
  }

  def isFormatting(f: Fragment) = f.description match {
    case Start    => true
    case End      => true
    case Br       => true
    case Tab(_)     => true
    case Backtab(_) => true
    case _          => false
  }

  def fragmentType(f: Fragment) =
    if (isExample(f))     "Example"
    else if (isText(f))   "Text"
    else if (isMarker(f)) "Marker"
    else                  "Other"

  /** iterate over elements to create a Fragments object */
  def foreach[T](seq: Seq[T])(f: T => Fragment): Fragments =
    seq.foldLeft(Fragments.empty) { (res, cur) => res.append(f(cur)) }

}


