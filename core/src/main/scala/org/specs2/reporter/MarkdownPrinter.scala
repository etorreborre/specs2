package org.specs2
package reporter

import control._
import Actions._
import io._
import execute._
import main.Arguments
import specification.core._

/**
 * This trait is not a full fledged markdown printer yet
 */
trait MarkdownPrinter extends Printer {

  def prepare(env: Env, specifications: List[SpecStructure]): Action[Unit] =
    env.fileSystem.mkdirs(MarkdownOptions.create(env.arguments).outDir)

  def finalize(env: Env, specifications: List[SpecStructure]): Action[Unit] =
    Actions.unit

  /** @return a Fold for the markdown output */
  def sink(env: Env, spec: SpecStructure): AsyncSink[Fragment] = {
    val options = MarkdownOptions.create(env.arguments)
    val path = options.outDir / FilePath.unsafe(spec.header.className+"."+options.extension)
    FoldIo.printToFilePath[ActionStack, Fragment](path)(f => fragmentToLine(options)(f))
  }

  def fragmentToLine(options: MarkdownOptions)(fragment: Fragment): Action[String] = {
    fragment match {
      case t if Fragment.isText(t) => protect(t.description.show)

      case e if Fragment.isExample(e) =>
        val description = e.description.show
        
        e.executionResult.map {
          case r: Success              => showDescription(description, r)
          case r @ Failure(m, _, _, _) => showDescription(description, r) + "\n  " + m
          case r @ Error(_, e1)        => showDescription(description, r) + "\n  " + e1
          case r: Skipped              => showDescription(description, r) + "\n  " + r.message
          case r: Pending              => showDescription(description, r) + " - "  + r.message
          case r                       => description + "\n" + r.message
        }

      case f if Fragment.isStepOrAction(f) =>
        f.executionResult map {
          case Failure(m, _, _, _) => "Step failed "+m
          case Error(_, e)         => "Step error "+e
          case _                   => ""
        }

      case Fragment(ref: SpecificationRef,_,_) => protect(toMarkdown(ref, options))
      case _                                   => ok("")
    }
  }

  def showDescription(description: String, result: Result): String =
    if (Seq("*", "-").exists(description.trim.startsWith))
      description
    else
      result.status+" "+description

  def toMarkdown(ref: SpecificationRef, options: MarkdownOptions) =
    s"[${ref.linkText}](${options.outDir / FilePath.unsafe(ref.url)})"

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
