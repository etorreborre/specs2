package org.specs2
package reporter

import control._
import data.Fold
import io._
import execute._
import foldm.stream.FoldProcessM._
import main.Arguments
import specification.core._
import scalaz.Show

/**
 * This trait is not a full fledged markdown printer yet
 */
trait MarkdownPrinter extends Printer {

  def prepare(env: Env, specifications: List[SpecStructure]): Action[Unit] =
    env.fileSystem.mkdirs(MarkdownOptions.create(env.arguments).outDir)

  def finalize(env: Env, specifications: List[SpecStructure]): Action[Unit] =
    Actions.unit

  /** @return a Fold for the markdown output */
  def sink(env: Env, spec: SpecStructure): SinkTask[Fragment] = {
    val options = MarkdownOptions.create(env.arguments)
    val show = MarkdownFragmentShow(options)
    val sink = Fold.showToFilePath(options.outDir / FilePath.unsafe(spec.header.className+"."+options.extension))(show)
    fromSink(sink)
  }

  def fragmentToLine(options: MarkdownOptions) = { fragment: Fragment =>
    fragment match {
      case t if Fragment.isText(t) => t.description.show

      case e if Fragment.isExample(e) =>
          e.executionResult match {
            case r: Success                       => showDescription(e)
            case f1 @ Failure(m, e1, st, details) => showDescription(e)+"\n  "+m
            case er @ Error(m, e1)                => showDescription(e)+"\n  "+e1
            case r: Skipped                       => showDescription(e)+"\n  "+r.message
            case r: Pending                       => showDescription(e)+" - " +r.message
            case r                                => e.description.show+"\n"+r.message
          }

      case f if Fragment.isStepOrAction(f) =>
        f.executionResult match {
          case f1 @ Failure(m, e1, st, details) => "Step failed "+m
          case er @ Error(m, e1)                => "Step error "+m
          case other                            => ""
        }

      case Fragment(ref: SpecificationRef,_,_) => toMarkdown(ref, options)
      case other                               => ""
    }
  }

  def showDescription(example: Fragment): String = {
    val description = example.description.show
    if (Seq("*", "-").exists(description.trim.startsWith)) description
    else example.executionResult.status+" "+description
  }


  def toMarkdown(ref: SpecificationRef, options: MarkdownOptions) =
    s"[${ref.linkText}](${options.outDir / FilePath.unsafe(ref.url)})"

  implicit def MarkdownFragmentShow(options: MarkdownOptions): Show[Fragment] = new Show[Fragment] {
    override def shows(f: Fragment): String =
      fragmentToLine(options)(f)
  }
}

object MarkdownPrinter extends MarkdownPrinter

case class MarkdownOptions(
  outDir: DirectoryPath,
  extension: String
)

object MarkdownOptions {

  /** create markdown options from arguments */
  def create(arguments: Arguments): MarkdownOptions =
    MarkdownOptions(
      outDir    = arguments.commandLine.directoryOr("markdown.outdir", outDir),
      extension = arguments.commandLine.valueOr("markdown.ext", extension)
    )

  val outDir    = "target" / "specs2-reports"
  val extension = "md"
}
