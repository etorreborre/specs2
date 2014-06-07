package org.specs2
package specification
package core

import scalaz.concurrent.Task
import execute.{AsResult, Skipped, Result}
import shapeless._
import Fragments._
import scalaz.Show
import scalaz.syntax.show._
import scalaz.stream.Process
import Process._
import control._
import text.Trim._
import Fragment._

case class Fragments(contents: Process[Task, Fragment]) {
  def update(f: Process[Task, Fragment] => Process[Task, Fragment]) = contentsLens.modify(this)(f)
  def flatMap(f: Fragment => Process[Task, Fragment])               = contentsLens.modify(this)(_ flatMap f)
  def |> (other: Process1[Fragment, Fragment])                      = contentsLens.modify(this)(_ |> other)
  def fragments: IndexedSeq[Fragment]                               = contents.runLog.run
  def filter(predicate: Fragment => Boolean)                        = contentsLens.modify(this)(_ filter predicate)

  def append(other: Process[Task, Fragment]): Fragments = contentsLens.modify(this)(_ fby other)
  def append(other: Fragment): Fragments                = append(Process(other))
  def append(others: Seq[Fragment]): Fragments          = append(Fragments(others:_*))
  def append(others: Fragments): Fragments              = append(others.contents)

  def prepend(other: Process[Task, Fragment]): Fragments = contentsLens.modify(this)(other fby _)
  def prepend(other: Fragment): Fragments                = prepend(Process(other))
  def prepend(others: Fragments): Fragments              = prepend(others.contents)
  def prepend(others: Seq[Fragment]): Fragments          = prepend(Fragments(others:_*))

  def when(condition: =>Boolean) = contentsLens.modify(this)(_  when emit(condition))

  def texts    = fragments.filter(isText)
  def examples = fragments.filter(isExample)
}

object Fragments {
  val contentsLens = lens[Fragments] >> 'contents
  def apply(fragments: Fragment*): Fragments = Fragments(emitAll(fragments).toSource)

  def trimFirstText(fs: Seq[Fragment]): Vector[Fragment] = fs match {
    case Fragment(RawText(t), e, l) +: rest => Fragment(RawText(t.trimStart), e, l) +: rest.toVector
    case other => other.toVector
  }
}

case class Fragment(description: Description, execution: Execution, location: Location = StacktraceLocation()) {

  def executionResult = execution.result
  def isRunnable = execution.isRunnable
  def mustStopOn(r: Result) = execution.nextMustStopIf(r)

  def stopOn(r: Result) = updateExecution(_.stopNextIf(r))
  def stopOnFail        = stopOn(org.specs2.execute.Failure())
  def stopOnSkipped     = stopOn(Skipped())
  def stopWhen(f: Result => Boolean) = updateExecution(_.stopNextIf(f))
  def join              = updateExecution(_.join)
  def isolate           = updateExecution(_.makeGlobal)
  def makeGlobal(when: Boolean)           = updateExecution(_.makeGlobal(when))
  def skip              = updateExecution(_.skip)
  def updateExecution(f: Execution => Execution) = copy(execution = f(execution))
  def setExecution(e: Execution) = updateExecution(_ => e)
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

  val Empty = Fragment(NoText, Execution.NoExecution)

  /** this allows the creation of fragments with a for loop */
  def foreach[T](seq: Seq[T])(f: T => Fragment): Fragment = {
    seq foreach f
    Empty
  }

  def isText(f: Fragment) = (f.description match {
    case t: RawText => true
    case t: Code    => true
    case _          => false
  }) && !f.isRunnable

  def isExample(f: Fragment) = (f.description match {
    case t: RawText => true
    case t: Code    => true
    case _          => false
  }) && f.isRunnable

  def isStepOrAction(f: Fragment) =
    (f.description == NoText) && f.isRunnable

  def isStep(f: Fragment) =
    isStepOrAction(f) && f.execution.mustJoin

  def isAction(f: Fragment) =
    isStepOrAction(f) && !f.execution.mustJoin

  def isTag(f: Fragment) = f.description match {
    case m: Marker => true
    case _         => false
  }

  def isLink(f: Fragment) = f.description match {
    case l: SpecificationLink => true
    case _                    => false
  }

  def specificationLink: PartialFunction[Fragment, SpecificationLink] = {
    case Fragment(l: SpecificationLink,_,_) => l
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
    if (isExample(f))   "Example"
    else if (isText(f)) "Text"
    else if (isTag(f))  "Tag"
    else                "Other"
}

object Results {
  /** this allows the creation of expectations with a for loop */
  def foreach[T, R : AsResult](seq: Seq[T])(f: T => R): Result = {
    seq foreach f
    org.specs2.execute.Success()
  }
}

