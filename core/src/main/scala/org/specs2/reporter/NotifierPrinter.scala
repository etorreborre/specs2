package org.specs2
package reporter

import text.NotNullStrings.*
import main.Arguments
import execute.{NoDetails, Result}
import control.*
import fp.*, syntax.*
import control.origami.*
import specification.core.*
import time.SimpleTimer
import main.Arguments

/**
 * A Printer can be created from a Notifier implementation
 */
case class NotifierPrinter(commandLineArguments: Arguments):

  /**
   * create a printer from a notifier
   */
  def printer(notifier: Notifier) = new Printer {
    def prepare(specifications: List[SpecStructure]): Action[Unit]  = Action.unit
    def finalize(specifications: List[SpecStructure]): Action[Unit] = Action.unit

    def sink(spec: SpecStructure): AsyncSink[Fragment] =
      val nf: Fold[Action, Fragment, Notified] { type S = Notified } =
            notifyFold.into[Action].startWith(Action.pure(notifier.specStart(spec.name, ""))).
              endWith(Action.pure(notifier.specEnd(spec.name, "")))

      nf.observeWithNextState(notifySink(spec, notifier)).void
  }

  def notifyFold: FoldState[Fragment, Notified] = new Fold[Id, Fragment, Notified] {
    type S = Notified
    val monad = Monad.idMonad

    def start = Notified(context = "start", start = false, close = false, hide = true)

    def fold = (ps: S, f: Fragment) => {
      // if the previous state was defining the closing of a block
      // the new state must not define a close action
      val s = if ps.close then ps.copy(close = false) else ps
      f match
        // a block start. The next text is the "context" name
        case Fragment(Start,_,_) => s.copy(start = true, close = false, hide = true)
        // a block start. The "context" name is the current block name
        case Fragment(End,_ ,_) => s.copy(start = false, close = true, hide = true)

        case f1 if Fragment.isText(f1) =>
          if s.start then s.copy(context = f1.description.show, start = true, hide = false)
          else         s.copy(context = f1.description.show, start = false, hide = false)

        case f1 if Fragment.isExample(f1) => s.copy(start = false, hide = false)
        case f1 if Fragment.isStep(f1)    => s.copy(start = false, hide = false)
        case _                            => s.copy(hide = true)
    }

    def end(s: S) = s
  }

  def notifySink(spec: SpecStructure, notifier: Notifier): AsyncSink[(Fragment, Notified)] =
    val arguments = commandLineArguments.overrideWith(spec.arguments)
    Folds.fromSink { case (f, n) => printFragment(notifier, f, n, arguments) }

  def printFragment(n: Notifier, f: Fragment, notified: Notified, args: Arguments): Action[Unit] =
    f.executedResult.map { er =>
        val location = f.location.show

        if !notified.hide then
          if notified.start then n.contextStart(notified.context.trim, location)
          else
            if Fragment.isExample(f) then   notifyExample(n, f, er, args)
            else if Fragment.isStep(f) then notifyStep(n, f, er, args)
            else if Fragment.isText(f) then notifyText(n, f, args)
        else if notified.close then n.contextEnd(notified.context.trim, location)
    }

  private def notifyExample(n: Notifier, f: Fragment, executedResult: ExecutedResult, args: Arguments) =
    val description = f.description.show.trim
    val location = f.location.show

    n.exampleStarted(description, location)

    notifyResult(executedResult.result, n, description, location, executedResult.timer.totalMillis)

  private def notifyResult(result: Result, n: Notifier, description: String, location: String, duration: Long): Unit =
    result match
      case r: execute.Success =>
        n.exampleSuccess(description, duration)

      case r: execute.Failure =>
        n.exampleFailure(description, r.message, location, r.exception, r.details, duration)

      case r: execute.Error =>
        n.exampleError(description, r.message, location, r.exception, duration)

      case r: execute.Skipped =>
        n.exampleSkipped(description, r.message, location, duration)

      case r: execute.Pending =>
        n.examplePending(description, r.message, location, duration)

      case execute.DecoratedResult(_, r2) =>
        notifyResult(r2, n, description, location, duration)

  private def notifyStep(n: Notifier, f: Fragment, executedResult: ExecutedResult, args: Arguments) =
    val location = f.location.show
    try
      n.stepStarted(location)

      def notifyResult(result: Result, timer: SimpleTimer): Unit =
        result match
          case r: execute.Success => n.stepSuccess(timer.totalMillis)
          case r: execute.Failure => n.stepError(r.message, location, r.exception, timer.totalMillis)
          case r: execute.Error   => n.stepError(r.message, location, r.exception, timer.totalMillis)
          case _ => ()
      notifyResult(executedResult.result, executedResult.timer)
      // catch AbstractMethod errors coming from Intellij since
      // calling new "step" methods on the Notifier interface is not supported yet
    catch
      case e: AbstractMethodError if e.getMessage.notNull.contains("Specs2Notifier") =>
        // if steps are not supported print failures and errors as examples failures and errors
        executedResult.result match
          case r: execute.Failure =>
            n.exampleFailure("step", r.message, location, r.exception, NoDetails, executedResult.timer.totalMillis)
          case r: execute.Error =>
            n.exampleError("step", r.message, location, r.exception, executedResult.timer.totalMillis)
          case _ =>
            ()

      case other: Throwable => throw other

  def notifyText(n: Notifier, f: Fragment, args: Arguments) =
    val description = f.description.show.trim
    val location = f.location.show

    n.text(description, location)

  case class Notified(context: String = "", start: Boolean = false, close: Boolean = false, hide: Boolean = false)
