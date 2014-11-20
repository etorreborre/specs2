package org.specs2
package reporter

import control._
import io._
import specification.core.Env
import scalaz.std.anyVal._

/** Representation of the Pandoc executable */
case class Pandoc(verbose: Boolean, executable: FilePath, inputFormat: String, outputFormat: String) {
  def isExecutableAvailable: Action[Unit] =
    Executable.run(executable, Seq("--version"))

}

object Pandoc {
  val executable   = FilePath("pandoc")
  val inputFormat  = "markdown+pipe_tables+auto_identifiers+header_attributes"
  val outputFormat = "html"

  /** build command-line arguments for Pandoc */
  def arguments(bodyPath: FilePath, templatePath: FilePath, variables: Map[String, String], outputFile: FilePath, options: Pandoc): Seq[String] = {
    val variablesOption = variables.flatMap { case (k, v) => Seq("-V", s"$k=$v") }

    Seq(bodyPath.path,
      "-f", options.inputFormat,
      "-t", options.outputFormat,
      "--template", templatePath.path,
      "-s", "-S",
      "-o", outputFile.path) ++
      variablesOption
  }

  /** @return the Pandoc executable if available */
  def getPandoc(env: Env): Action[Option[Pandoc]] = {
    import env.arguments.commandLine._
    val markdown = boolOr("pandoc", true)

    if (markdown) {
      val pandoc = Pandoc(
        verbose      = boolOr("pandoc.verbose", false),
        executable   = fileOr("pandoc.exec", Pandoc.executable),
        inputFormat  = valueOr("pandoc.inputformat", Pandoc.inputFormat),
        outputFormat = valueOr("pandoc.outputformat", Pandoc.outputFormat))

      pandoc.isExecutableAvailable.map(_ => Option(pandoc)).orElse(
        Actions.fail[Option[Pandoc]]("the pandoc executable is not available at: "+pandoc.executable.path))
    }

    else Actions.ok(None)
  }

}


