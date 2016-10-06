package org.specs2
package reporter

import text.NotNullStrings._
import main.Arguments
import execute.Result

import scalaz.syntax.show._

import control._
import eff._, all._
import control.origami._
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

    def sink(env: Env, spec: SpecStructure): AsyncSink[Fragment] = {
      (notifyFold.into[ActionStack]
      	.startWith(asyncDelay(notifier.specStart(spec.name, "")))
      	.endWith(asyncDelay(notifier.specEnd(spec.name, ""))).observeWithNextState(notifySink(spec, notifier, env.arguments))).void
    }
  }

  def notifyFold: FoldState[Fragment, Notified] = new Fold[NoFx, Fragment, Notified] {
    type S = Notified

    def start = pure[NoFx, S](Notified(context = "start", start = false, close = false, hide = true))

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

    def end(s: S) = pure[NoFx, S](s)
  }

  def notifySink(spec: SpecStructure, notifier: Notifier, args: Arguments): AsyncSink[(Notified, Fragment)] =
    new Fold[ActionStack, (Notified, Fragment), Unit] {
      type S = Unit
      def start = pure(())
      def fold = (s: S, a: (Notified, Fragment)) => printFragment(notifier, a._2, a._1, args)
      def end(s: S) = pure(s)
    }

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


