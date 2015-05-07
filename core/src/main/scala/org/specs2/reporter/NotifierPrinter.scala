package org.specs2
package reporter

import foldm._, FoldId._, FoldM._
import stream.FoldProcessM._
import main.Arguments
import control._
import execute.Result
import specification._
import scalaz.Id, Id._
import scalaz.concurrent.Task
import scalaz.stream._
import scalaz.syntax.show._
import scalaz.syntax.functor._
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

    def sink(env: Env, spec: SpecStructure): SinkTask[Fragment] =
      (notifyFold.into[Task] <<<* fromSink(notifySink(spec, notifier, env.arguments))).as(())
  }

  def notifyFold: FoldState[Fragment, Notified] = new FoldM[Fragment, Id, Notified] {
    type S = Notified

    def start = Notified(context = "start", start = false, close = false, hide = true)

    def fold = (s: S, f: Fragment) => f match {
      // a block start. The next text is the "context" name
      case Fragment(Start,_,_) => s.copy(start = true, close = false, hide = true)
      // a block start. The "context" name is the current block name
      case Fragment(End,_ ,_) => s.copy(start = false, close = true, hide = true)
          
      case f1 if Fragment.isText(f1) =>
        if (s.start) s.copy(context = f1.description.shows, start = true, hide = false)
        else         s.copy(context = f1.description.shows, start = false, hide = false)

      case f1 if Fragment.isExample(f1) => s.copy(start = false, hide = false)
      case _                            => s.copy(hide = true)
    }

    def end(s: S) = s
  }

  case class Notified(context: String, start: Boolean, close: Boolean, hide: Boolean)

  def notifySink(spec: SpecStructure, notifier: Notifier, args: Arguments): Sink[Task, (Notified, Fragment)] =
    io.resource(Task.delay { notifier.specStart(spec.name, ""); notifier})(
      (n: Notifier) => Task.delay(n.specEnd(spec.name, "")))(
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

<<<<<<< HEAD
              notifyResult(f.executionResult)
            } else if (Fragment.isStep(f)) {
              try {
                n.stepStarted(location)

                def notifyResult(result: Result): Unit =
                  result match {
                    case r: execute.Success => n.stepSuccess(duration(f))
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
=======
          notifyResult(f.executionResult)
        } else if (Fragment.isText(f)) n.text(description, location)
>>>>>>> refactors printers using foldm
      }
    } else if (notified.close) n.contextEnd(notified.context.trim, location)
  }

}


