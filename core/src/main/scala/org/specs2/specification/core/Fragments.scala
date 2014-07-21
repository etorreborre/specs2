package org.specs2
package specification
package core

import data.Processes
import scalaz.concurrent.Task
import execute.{AsResult, Skipped, Result}
import shapeless._
import Fragments._
import scalaz.Show
import scalaz.syntax.show._
import scalaz.stream._
import Process._
import control._
import org.specs2.text.Trim._
import Fragment._

case class Fragments(contents: Process[Task, Fragment]) {
  def append(other: Fragment): Fragments                = append(Process(other))
  def append(others: Seq[Fragment]): Fragments          = append(Fragments(others:_*))
  def append(others: Fragments): Fragments              = append(others.contents)
  def appendLazy(other: =>Fragment): Fragments          = append(Process.eval(Task.delay(other)))

  def prepend(other: Fragment): Fragments                = prepend(Process(other))
  def prepend(others: Fragments): Fragments              = prepend(others.contents)
  def prepend(others: Seq[Fragment]): Fragments          = prepend(Fragments(others:_*))
  def prependLazy(other: =>Fragment): Fragments          = prepend(Process.eval(Task.delay(other)))

  def when(condition: =>Boolean) = contentsLens.modify(this)(_  when emit(condition))

  def map(f: Fragment => Fragment)                                  = contentsLens.modify(this)(_ map f)
  def mapDescription(f: Description => Description)                 = map(_.updateDescription(f))
  def filter(predicate: Fragment => Boolean)                        = contentsLens.modify(this)(_ filter predicate)

  def update(f: Process[Task, Fragment] => Process[Task, Fragment]) = contentsLens.modify(this)(f)
  def flatMap(f: Fragment => Process[Task, Fragment])               = contentsLens.modify(this)(_ flatMap f)
  def |> (other: Process1[Fragment, Fragment])                      = contentsLens.modify(this)(_ |> other)
  def append(other: Process[Task, Fragment]): Fragments             = contentsLens.modify(this)(_ fby other)
  def prepend(other: Process[Task, Fragment]): Fragments            = contentsLens.modify(this)(other fby _)

  def fragments: IndexedSeq[Fragment] = contents.runLog.run
  def texts                           = fragments.filter(isText)
  def examples                        = fragments.filter(isExample)

  def stripMargin: Fragments = stripMargin('|')
  def stripMargin(margin: Char): Fragments = mapDescription(_.stripMargin(margin))

  def compact = {
    val (text1, accumulated1) = fragments.foldLeft((None, Vector()): (Option[String], Seq[Fragment])) { case ((text, accumulated), fragment) =>
      fragment match {
        case Fragment(Text(t),l, e) if isText(fragment) =>
          (text.map(_+t).orElse(Some(t)), accumulated)

        case other =>
          text match {
            case Some(t1) => (None, accumulated ++ Seq(Fragment(Text(t1), Execution.NoExecution), other))
            case None     => (None, accumulated :+ other)
          }
      }
    }
    val compacted = text1.fold(accumulated1)(t => accumulated1 :+ Fragment(Text(t), Execution.NoExecution))
    Fragments(compacted:_*)
  }
  // withNext is not playing well with scan :-(
  /*
    update { p: Process[Task, Fragment] =>
    (p |> Processes.withNext[Fragment]).scan((None, Vector()): (Option[String], Seq[Fragment])) { case ((text, accumulated), (fragment, next)) =>
      println((fragment, next))
      (fragment, next) match {
        case (Fragment(Text(t),l, e), Some(_)) if isText(fragment) =>
          println("case 1")
          (text.map(_+t).orElse(Some(t)), accumulated)

        case (other, Some(_)) =>
          println("case 2")
          text match {
            case Some(t1) => (None, Seq(Fragment(Text(t1), Execution.NoExecution), other))
            case None     => (None, Seq(other))
          }

        case (Fragment(Text(t), e, l), None) if isText(fragment) =>
          println("case 3")
          text match {
            case Some(t1) => (None, Seq(Fragment(Text(t1+t), e, l)))
            case None     => (None, Seq(fragment))
          }

        case (other, None) =>
          println("case 4")
          text match {
            case Some(t1) => (None, Seq(Fragment(Text(t1), Execution.NoExecution), other))
            case None     => (None, Seq(other))
          }
      }
    }.flatMap { case (_, fs) => emitSeq(fs) }
  }
  */
}

object Fragments {
  val contentsLens = lens[Fragments] >> 'contents

  def apply(fragments: Fragment*): Fragments = Fragments(emitAll(fragments).toSource)
}

case class Fragment(description: Description, execution: Execution, location: Location = StacktraceLocation()) {

  def executionResult = execution.result
  def isExecuted = execution.isExecuted
  def isRunnable = execution.isRunnable
  def mustStopOn(r: Result) = execution.nextMustStopIf(r)

  def stopOn(r: Result) = updateExecution(_.stopNextIf(r))
  def stopOnError       = stopWhen(_.isError)
  def stopOnFail        = stopWhen(_.isFailure)
  def stopOnSkipped     = stopWhen(_.isSkipped)

  def stopWhen(f: Result => Boolean) = updateExecution(_.stopNextIf(f))
  def join              = updateExecution(_.join)
  def isolate           = updateExecution(_.makeGlobal)
  def makeGlobal(when: Boolean)           = updateExecution(_.makeGlobal(when))
  def skip              = updateExecution(_.skip)
  def updateExecution(f: Execution => Execution) = copy(execution = f(execution))
  def updateRun(r: (Env => Result) => (Env => Result)) = updateExecution(_.updateRun(r))

  def updateDescription(f: Description => Description) = copy(description = f(description))
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

  def isText(f: Fragment) = (f.description match {
    case t: Text => true
    case _          => false
  }) && !f.isRunnable

  def isExample(f: Fragment) = (f.description match {
    case t: Text => true
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

