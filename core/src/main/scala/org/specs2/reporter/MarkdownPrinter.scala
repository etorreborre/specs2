package org.specs2
package reporter

import control._
import data.Fold
import io._
import org.specs2.execute._
import org.specs2.main.Arguments
import org.specs2.specification.core._
import scalaz.concurrent.Task
import scalaz.stream._
import scalaz.Show

/**
 * This trait is not a full fledged markdown printer yet
 */
trait MarkdownPrinter extends Printer {

  def prepare(env: Env, specifications: List[SpecificationStructure]): Action[Unit] =
    env.fileSystem.mkdirs(MarkdownOptions.create(env.arguments).outDir)

  def finalize(env: Env, specifications: List[SpecificationStructure]): Action[Unit] =
    Actions.unit

  /** @return a Fold for the markdown output */
  def fold(env: Env, spec: SpecStructure): Fold[Fragment] = new Fold[Fragment] {
    type S = Unit

    val options = MarkdownOptions.create(env.arguments)

    lazy val sink: Sink[Task, (Fragment, Unit)] =
      Fold.showToFilePath(options.outDir / FilePath.unsafe(spec.header.className+"."+options.extension))(MarkdownFragmentShow(options))

    def prepare = Task.now(())

    def fold = Fold.unitFoldFunction
    def init = Task.now(())

    def last(u: Unit) =
      Task.now(())

    implicit def MarkdownFragmentShow(options: MarkdownOptions): Show[Fragment] = new Show[Fragment] {
      override def shows(f: Fragment): String =
        fragmentToLine(options)(f)
    }
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

      case Fragment(link: SpecificationLink,_,_) => toMarkdown(link, options)
      case other                                 => ""
    }
  }

  def showDescription(example: Fragment): String = {
    val description = example.description.show
    if (Seq("*", "-").exists(description.trim.startsWith)) description
    else example.executionResult.status+" "+description
  }


  def toMarkdown(link: SpecificationLink, options: MarkdownOptions) =
    s"[${link.linkText}](${options.outDir / FilePath.unsafe(link.url)})"

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