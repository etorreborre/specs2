package org.specs2
package reporter

import specification.core._
import data.Fold
import specification.process.{Stats, Statistics}
import io._
import main.Arguments
import scalaz.concurrent.Task
import scalaz.syntax.bind._
import control._
import java.util.regex.Pattern._
import java.net.{JarURLConnection, URL}
import scalaz.std.list._
import scalaz.std.anyVal._
import scalaz.syntax.traverse._
import scalaz.syntax.bind._
import html.HtmlTemplate
import HtmlBodyPrinter._
import Pandoc._

/**
 * Printer for html files
 */
trait HtmlPrinter extends Printer {

  /** @return a Fold for the Html output */
  def fold(env: Env, spec: SpecStructure): Fold[Fragment] = new Fold[Fragment] {
    type S = Stats

    lazy val sink = Fold.unitSink[Fragment, Stats]

    def prepare = Task.now(())
    def fold = Statistics.fold
    def init = Stats.empty

    def last(stats: Stats) = {
      val action =
        getPandoc(env).flatMap {
          case None         => printHtml(env, spec, stats)
          case Some(pandoc) => printHtmlWithPandoc(env, spec, stats, pandoc)
        }
      action.toTask
    }
  }

  /**
   * WITHOUT PANDOC
   */

  /**
   * Print the execution results as an Html file
   *
   *  - copy resources: css, javascript, template
   *  - create the file content using the template
   *  - output the file
   */
  def printHtml(env: Env, spec: SpecStructure, stats: Stats): Action[Unit] = {
    import env.fileSystem._
    for {
      options  <- getHtmlOptions(env.arguments)
      _        <- copyResources(env, options.outDir)
      template <- readFile(options.template)
      content  <- makeHtml(template, spec, stats, options, env.arguments)
      _        <- writeFile(outputFilePath(options.outDir, spec), content)
    } yield ()
  }

  /**
   * Get html options, possibly coming from the command line
   */
  def getHtmlOptions(arguments: Arguments): Action[HtmlOptions] = {
    import arguments.commandLine._
    val out = directoryOr("html.outdir", HtmlOptions.outDir)
    Actions.ok(HtmlOptions(
      outDir    = out,
      baseDir   = directoryOr("html.basedir",   HtmlOptions.baseDir),
      template  = fileOr(     "html.template",  HtmlOptions.template(out)),
      variables = mapOr(      "html.variables", HtmlOptions.variables),
      noStats   = boolOr(     "html.nostats",   HtmlOptions.noStats)))
  }


  /**
   * Create the html file content from:
   *
   *  - the template
   *  - the body of the file (built from the specification execution)
   */
  def makeHtml(template: String, spec: SpecStructure, stats: Stats, options: HtmlOptions, arguments: Arguments): Action[String] = {
    val body = makeBody(spec, stats, options, arguments, pandoc = true)
    val variables1 =
      options.variables
        .updated("body", body)
        .updated("title", spec.wordsTitle)
        .updated("baseDir", options.baseDir.path)
        .updated("outDir", options.outDir.path)
    HtmlTemplate.runTemplate(template, variables1)
  }

  /**
   * WITH PANDOC
   */

  /**
   * Print the execution results as an Html file
   *
   *  - copy resources: css, javascript, template
   *  - create the file content using the template and Pandoc (as an external process)
   */
  def printHtmlWithPandoc(env: Env, spec: SpecStructure, stats: Stats, pandoc: Pandoc): Action[Unit] = {
    import env.fileSystem._

    for {
      options  <- getHtmlOptions(env.arguments)
      _        <- copyResources(env, options.outDir)
      _        <- withEphemeralFile(options.outDir | options.template.name) {
                    copyFile(options.outDir)(options.template) >>
                    makePandocHtml(spec, stats, pandoc, options, env)
                  }
    } yield ()
  }

  /**
   * Create the Html file by invoking Pandoc
   */
  def makePandocHtml(spec: SpecStructure, stats: Stats, pandoc: Pandoc, options: HtmlOptions, env: Env): Action[Unit] =  {
    import env.fileSystem._

    val variables1 =
      options.variables
        .updated("title", spec.wordsTitle)
        .updated("baseDir", options.baseDir.path)
        .updated("outDir", options.outDir.path)

    val bodyFile: FilePath =
      options.outDir | FileName.unsafe("body-"+spec.hashCode)

    val pandocArguments = Pandoc.arguments(bodyFile, options.template, variables1, outputFilePath(options.outDir, spec), pandoc)

    withEphemeralFile(bodyFile) {
      writeFile(bodyFile, makeBody(spec, stats, options, env.arguments, pandoc = true)) >>
      Executable.run(pandoc.executable, pandocArguments)
    }
  }

  def outputFilePath(directory: DirectoryPath, spec: SpecStructure): FilePath =
    directory | FileName.unsafe(spec.specClassName+".html")

  def copyResources(env: Env, outDir: DirectoryPath): Action[List[Unit]] = {
    env.fileSystem.mkdirs(outDir) >>
      List(DirectoryPath("css"),
        DirectoryPath("javascript"),
        DirectoryPath("images"),
        DirectoryPath("templates")).
        map(copySpecResourcesDir(env, "org" / "specs2" / "reporter", outDir, classOf[HtmlPrinter].getClassLoader)).sequenceU
  }

  def copySpecResourcesDir(env: Env, base: DirectoryPath, outputDir: DirectoryPath, loader: ClassLoader)(src: DirectoryPath): Action[Unit] = {
    Option(loader.getResource((base / src).path)) match {
      case None =>
        Actions.fail(s"no resource found for url ${(base / src).path}")

      case Some(url) =>
        val fs = env.fileSystem
        if (url.getProtocol.equalsIgnoreCase("jar"))
          fs.unjar(jarOf(url), outputDir, s"^${quote(base.path)}(/${quote(src.path)}/.*)$$")
        else
          fs.copyDir(DirectoryPath.unsafe(url.toURI), outputDir / src)
    }
  }

  private def jarOf(url: URL): URL = url.openConnection.asInstanceOf[JarURLConnection].getJarFileURL

}

object HtmlPrinter extends HtmlPrinter
