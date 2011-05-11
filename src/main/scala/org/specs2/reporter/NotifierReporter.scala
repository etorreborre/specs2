package org.specs2
package reporter

import execute._
import specification._
import Levels._
import specification.SpecificationStructure
import scalaz.{Tree, Scalaz}
import Scalaz._
import data.Trees._
import main.Arguments
import io.Location

/**
 * Report a Specification by notifying execution events to a Notifier
 */
trait NotifierReporter extends Reporter
    with DefaultSelection
    with DefaultSequence
    with DefaultExecutionStrategy
    with NotifierExporting

trait NotifierExporting extends Exporting {
  type ExportType = Unit
  val notifier: Notifier
  /** @return a function exporting ExecutedFragments */
  def export(s: SpecificationStructure)(implicit args: Arguments): Seq[ExecutedFragment] => ExportType = (fs: Seq[ExecutedFragment]) => {
    notifyExport(fs)
    if (args.contains("html")) new HtmlExporting {}.export(s)(args)(fs)
  }
  private def notifyExport(fs: Seq[ExecutedFragment])(implicit args: Arguments) = {
    def notify(fs: Seq[ExecutedFragment]) = {
      val tree = Levels.foldAll(fs).toTree(mapper)
      if (args.noindent) export(tree.flattenSubForests)
      else               export(tree)
    }

    if (fs.nonEmpty) notify(fs)
    else             notify(Seq(ExecutedSpecStart(SpecName("empty specification"), Arguments(), new Location()),
                                ExecutedSpecEnd(SpecName("empty specification"), new Location())))

  }
  private val mapper = (f: ExecutedFragment, i: Int) => f match {
    case e: ExecutedStandardFragment => None
    case other                       => Some(other)
  }
  private def export(tree: Tree[ExecutedFragment])(implicit args: Arguments) {
    tree.rootLabel match {
      case f @ ExecutedSpecStart(n, _, _)                                   => {
        notifier.specStart(n.name, f.location.toString)
        tree.subForest.foreach(export)
      }
      case f @ ExecutedSpecEnd(n, _)                                        => {
        notifier.specEnd(n.name, f.location.toString)
      }
      case f @ ExecutedText(t, _)  if tree.subForest.isEmpty && !args.xonly => notifier.text(t, f.location.toString)
      case f @ ExecutedText(t, _)                                           => {
        notifier.contextStart(t, f.location.toString)
        tree.subForest.foreach(export)
        notifier.contextEnd(t, f.location.toString)
      }
      case f @ ExecutedResult(s, r, t, l)                                  => {
        notifier.exampleStarted(s.toString, l.toString)
        r match {
          case Success(_)              if !args.xonly => notifier.exampleSuccess(s.toString, t.elapsed)
          case fail @ Failure(_,_,_,_)                => notifier.exampleFailure(s.toString, fail.message, fail.location, args.traceFilter(fail.exception), fail.details, t.elapsed)
          case err  @ Error(_,_)                      => notifier.exampleError(s.toString,   err.message, err.location, args.traceFilter(err.exception), t.elapsed)
          case Skipped(_,_)            if !args.xonly => notifier.exampleSkipped(s.toString, r.message, t.elapsed)
          case Pending(_)              if !args.xonly => notifier.examplePending(s.toString, r.message, t.elapsed)
          case other                                  => ()
        }
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

object ConsoleNotifier extends Notifier {
  def specStart(title: String, location: String)                                                                      = Console.println(Seq("specStart"     ,title  ,location)                       .mkString("; "))
  def specEnd(title: String, location: String)                                                                        = Console.println(Seq("specEnd"       ,title  ,location)                       .mkString("; "))
  def contextStart(text: String, location: String)                                                                    = Console.println(Seq("contextStart"  ,text   ,location)                       .mkString("; "))
  def contextEnd(text: String, location: String)                                                                      = Console.println(Seq("contextEnd"    ,text   ,location)                       .mkString("; "))
  def text(text: String, location: String)                                                                            = Console.println(Seq("text"          ,text   ,location)                       .mkString("; "))
  def exampleStarted(name: String, location: String)                                                                  = Console.println(Seq("exampleStarted",name   ,location)                       .mkString("; "))
  def exampleSuccess(name: String, duration: Long)                                                                    = Console.println(Seq("exampleSuccess",name   ,duration)                       .mkString("; "))
  def exampleFailure(name: String, message: String, location: String, f: Throwable, details: Details, duration: Long) = Console.println(Seq("exampleFailure",name   , message,location,f.getMessage, details, duration).mkString("; "))
  def exampleError  (name: String, message: String, location: String, f: Throwable, duration: Long)                   = Console.println(Seq("exampleError"  ,name   , message,location,f.getMessage, duration).mkString("; "))
  def exampleSkipped(name: String, message: String, duration: Long)                                                   = Console.println(Seq("exampleSkipped",name   , message,duration)                       .mkString("; "))
  def examplePending(name: String, message: String, duration: Long)                                                   = Console.println(Seq("examplePending",name   , message,duration)                       .mkString("; "))
}