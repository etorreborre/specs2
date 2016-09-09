package org.specs2
package reporter

import org.specs2.data.Processes
import text.NotNullStrings._
import foldm._, FoldM._
import stream.FoldProcessM._
import main.Arguments
import control._
import execute.Result
import scalaz.Id, Id._
import scalaz.concurrent.Task
import org.specs2.codata._
import scalaz.syntax.show._
import specification.core._

/**
 * A Printer can be created from a Notifier implementation
 */
object NotifierPrinter {

  /**
   * create a printer from a notifier
   */
  def printer(notifier: Notifier) = new Printer {
    def prepare(env: Env, specifications: List[SpecStructure]): Action[Unit]  = Actions.unit
    def finalize(env: Env, specifications: List[SpecStructure]): Action[Unit] = Actions.unit

    def sink(env: Env, spec: SpecStructure): SinkTask[Fragment] = {
      (notifyFold.into[Task]
      	.startWith(Task.delay(notifier.specStart(spec.name, "")))
      	.endWith(Task.delay(notifier.specEnd(spec.name, ""))) <<<* fromSink(notifySink(spec, notifier, env.arguments))).void}
  }

  def notifyFold: FoldState[Fragment, Notified] = new FoldM[Fragment, Id, Notified] {
    type S = Notified

    def start = Notified(context = "start", start = false, close = false, hide = true)

    def fold = (ps: S, f: Fragment) => {
      // if the previous state was defining the closing of a block
      // the new state must not define a close action
      val s = if (ps.close) ps.copy(close = false) else ps
      f match {
        // a block start. The next text is the "context" name
        case Fragment(Start,_,_) => s.copy(start = true, close = false, hide = true)
        // a block start. The "context" name is the current block name
        case Fragment(End,_ ,_) => s.copy(start = false, close = true, hide = true)

        case f1 if Fragment.isText(f1) =>
          if (s.start) s.copy(context = f1.description.shows, start = true, hide = false)
          else         s.copy(context = f1.description.shows, start = false, hide = false)

        case f1 if Fragment.isExample(f1) => s.copy(start = false, hide = false)
        case f1 if Fragment.isStep(f1)    => s.copy(start = false, hide = false)
        case _                            => s.copy(hide = true)
      }
    }

    def end(s: S) = s
  }

  def notifySink(spec: SpecStructure, notifier: Notifier, args: Arguments): Sink[Task, (Notified, Fragment)] =
    Processes.resource(Task.now(notifier))(
      (n: Notifier) => Task.now(()))(
      (n: Notifier) => Task.delay { case (block: Notified, f: Fragment) => Task.now(printFragment(n, f, block, args)) })


  def printFragment(n: Notifier, f: Fragment, notified: Notified, args: Arguments) = {
    val description = f.description.shows.trim

    val location = f.location.fullLocation(args.traceFilter).getOrElse("no location")
    def duration(f: Fragment) = f.execution.executionTime.totalMillis

    if (!notified.hide) {
      if (notified.start) n.contextStart(notified.context.trim, location)
      else {
        if (Fragment.isExample(f)) {
          n.exampleStarted(description, location)

          def notifyResult(result: Result): Unit =
            result match {
              case r: execute.Success =>
                n.exampleSuccess(description, duration(f))

              case r: execute.Failure =>
                n.exampleFailure(description, r.message, location, r.exception, r.details, duration(f))

              case r: execute.Error =>
                n.exampleError(description, r.message, location, r.exception, duration(f))

              case r: execute.Skipped =>
                n.exampleSkipped(description, r.message, location, duration(f))

              case r: execute.Pending =>
                n.examplePending(description, r.message, location, duration(f))

              case execute.DecoratedResult(_, r2) =>
                notifyResult(r2)
            }

          notifyResult(f.executionResult)
        } else if (Fragment.isStep(f)) {
          try {
            n.stepStarted(location)

            def notifyResult(result: Result): Unit =
              result match {
                case r: execute.Success => n.stepSuccess(duration(f))
                case r: execute.Failure => n.stepError(r.message, location, r.exception, duration(f))
                case r: execute.Error   => n.stepError(r.message, location, r.exception, duration(f))
                case _ => ()
              }
            notifyResult(f.executionResult)
            // catch AbstractMethod errors coming from Intellij since adding
            // calling new "step" methods on the Notifier interface is not supported yet
          } catch {
            case e: AbstractMethodError if e.getMessage.notNull.contains("JavaSpecs2Notifier") => ()
            case other: Throwable => throw other
          }

        } else if (Fragment.isText(f)) n.text(description, location)
      }
    } else if (notified.close) n.contextEnd(notified.context.trim, location)
  }

  case class Notified(context: String = "", start: Boolean = false, close: Boolean = false, hide: Boolean = false)
}


