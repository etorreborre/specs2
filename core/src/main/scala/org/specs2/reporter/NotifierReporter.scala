package org.specs2
package reporter

import execute._
import specification._
import Levels._
import scalaz.{Tree, Scalaz}
import data.Trees._
import main.Arguments

/**
 * Report a Specification by notifying execution events to a Notifier
 */
trait NotifierReporter extends DefaultReporter with NotifierExporting

trait NotifierExporting extends Exporting with Exporters {

  val notifier: Notifier
  /** @return a function exporting ExecutedFragments */
  def export(implicit args: Arguments): ExecutingSpecification => ExecutedSpecification = (spec: ExecutingSpecification) => {
    val executed = spec.execute
    notifyExport(executed.fragments)
    executed
  }

  private def notifyExport(fs: Seq[ExecutedFragment])(implicit args: Arguments) = {
    def notify(fs: Seq[ExecutedFragment]) = {
      val tree = Levels.foldAll(fs).toTree(mapper)
      export(tree)
    }

    if (fs.nonEmpty) notify(fs)
    else {             
      val empty = Fragments().specTitleIs(SpecName("empty specification"))
      Seq(ExecutedSpecStart(empty.specStart), ExecutedSpecEnd(empty.specEnd))
    }

  }

  private val mapper = (f: ExecutedFragment, i: Int) => f match {
    case e: ExecutedStandardFragment => None
    case other                       => Some(other)
  }

  private def export(tree: Tree[ExecutedFragment])(implicit args: Arguments) {
    tree.rootLabel match {
      case f @ ExecutedSpecStart(_,_,_)                                      => {
        notifier.specStart(f.name, f.location.toString)
        tree.subForest.foreach(export)
        notifier.specEnd(f.name, f.location.toString)
      }
      case f @ ExecutedText(t,_)  if tree.subForest.isEmpty => if (args.canShow("*")) notifier.text(t.t, f.location.toString)
      case f @ ExecutedText(t,_)                            => {
        if (args.canShow("*")) notifier.contextStart(t.t, f.location.toString)
        tree.subForest.foreach(export)
        if (args.canShow("*")) notifier.contextEnd(t.t, f.location.toString)
      }
      case f @ ExecutedResult(s, r, t, l, st)                               => {
        if (args.canShow(r.status)) notifier.exampleStarted(s.raw, l.toString)
        def notifyResult(result: Result) {
          result match {
            case Success(_,_)            => notifier.exampleSuccess(s.raw, t.totalMillis)
            case fail @ Failure(_,_,_,_) => notifier.exampleFailure(s.raw, args.removeColors(fail.message),
                                                                                    fail.location(args.traceFilter), args.traceFilter(fail.exception), fail.details, t.totalMillis)
            case err  @ Error(_,_)       => notifier.exampleError(s.raw, args.removeColors(err.message), err.location(args.traceFilter),
                                                                                   args.traceFilter(err.exception), t.totalMillis)
            case Skipped(_,_)            => notifier.exampleSkipped(s.raw, args.removeColors(r.message), t.totalMillis)
            case Pending(_)              => notifier.examplePending(s.raw, args.removeColors(r.message), t.totalMillis)
            case DecoratedResult(_, res) => notifyResult(res)
          }
        }
        if (args.canShow(r.status)) notifyResult(r)
      }
      case other                           => tree.subForest.foreach(export)
    }
  }
}

/**
 * This trait can be used for any event concerning the execution of examples
 * seen as a Tree of Fragments.
 *
 * A pair of contextStart/contextEnd calls delimits a sequence of children in that tree.
 */
trait Notifier {
  def specStart(title: String, location: String)
  def specEnd(title: String, location: String)
  def contextStart(text: String, location: String)
  def contextEnd(text: String, location: String)
  def text(text: String, location: String)
  def exampleStarted(name: String, location: String)
  def exampleSuccess(name: String, duration: Long)
  def exampleFailure(name: String, message: String, location: String, f: Throwable, details: Details, duration: Long)
  def exampleError  (name: String, message: String, location: String, f: Throwable, duration: Long)
  def exampleSkipped(name: String, message: String, duration: Long)
  def examplePending(name: String, message: String, duration: Long)
}

