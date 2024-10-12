package org.specs2
package reporter

import specification.core.*
import specification.process.{Statistics, Stats}
import io.*
import FileName.*
import main.Arguments
import control.*
import java.util.regex.Pattern.*
import java.net.{JarURLConnection, URL}

import fp.syntax.*
import HtmlBodyPrinter.*
import Pandoc.*
import html.*
import html.TableOfContents.*
import SpecHtmlPage.*
import concurrent.ExecutionEnv
import time.SimpleTimer
import origami.*, Folds.*
import HtmlPrinter.*

/** Printer for html files
  */
case class HtmlPrinter(env: Env, searchPage: SearchPage, logger: Logger = ConsoleLogger()) extends Printer:

  def prepare(specifications: List[SpecStructure]): Action[Unit] = Action.unit

  /** create an index for all the specifications, if required */
  def finalize(specifications: List[SpecStructure]): Action[Unit] = {
    getHtmlOptions(env.arguments) >>= { (options: HtmlOptions) =>
      searchPage.createIndex(env, specifications, options).when(options.search) >>
        createToc(env, specifications, options.outDir, options.tocEntryMaxSize, env.fileSystem).when(options.toc) >>
        reportMissingSeeRefs(specifications, options.outDir)(using env.specs2ExecutionEnv).when(
          options.warnMissingSeeRefs
        )
    }
  }.toAction

  /** @return a SinkTask for the Html output */
  def sink(spec: SpecStructure): AsyncSink[Fragment] =
    val copyHtmlResources = (getHtmlOptions(env.arguments) >>= (options => copyResources(env, options))).void.toAction
    val htmlSink = (Statistics.fold `zip` list[Fragment].into[Action] `zip` SimpleTimer.timerFold.into[Action])
      .mapFlatten { case ((stats, fragments), timer) =>
        val executedSpec = spec.copy(lazyFragments = () => Fragments(fragments*))
        getPandoc(env).flatMap {
          case Some(pandoc) => printHtmlWithPandoc(env, executedSpec, stats, timer, pandoc).toAction
          case _            => printHtml(env, executedSpec, stats, timer).toAction
        }
      }
    htmlSink.startWith(copyHtmlResources)

  /** WITHOUT PANDOC
    */

  /** Print the execution results as an Html file
    *
    *   - copy resources: css, javascript, template
    *   - create the file content using the template
    *   - output the file
    */
  def printHtml(env: Env, spec: SpecStructure, stats: Stats, timer: SimpleTimer): Operation[Unit] =
    import env.{fileSystem as fs}
    for
      options <- getHtmlOptions(env.arguments)
      template <- fs.readFile(options.template) ||| logger
        .warnAndFail[String]("No template file found at " + options.template.path, RunAborted)
      content <- makeHtml(template, spec, stats, timer, options, env.arguments)(env.specs2ExecutionEnv)
      _ <- fs.writeFile(outputPath(options.outDir, spec), content)
    yield ()

  /** Get html options, possibly coming from the command line
    */
  def getHtmlOptions(arguments: Arguments): Operation[HtmlOptions] =
    import arguments.commandLine.*
    val out = directoryOr("html.outdir", HtmlOptions.outDir)
    Operation.ok(
      HtmlOptions(
        outDir = out,
        baseDir = directoryOr("html.basedir", HtmlOptions.baseDir),
        template = fileOr("html.template", HtmlOptions.template(out)),
        variables = mapOr("html.variables", HtmlOptions.variables),
        noStats = boolOr("html.nostats", HtmlOptions.noStats),
        search = boolOr("html.search", HtmlOptions.search),
        toc = boolOr("html.toc", HtmlOptions.toc),
        tocEntryMaxSize = intOr("html.toc.entrymaxsize", HtmlOptions.tocEntryMaxSize),
        warnMissingSeeRefs = boolOr("html.warn.missingseerefs", HtmlOptions.warnMissingSeeRefs)
      )
    )

  /** Create the html file content from:
    *
    *   - the template
    *   - the body of the file (built from the specification execution)
    */
  def makeHtml(
      template: String,
      spec: SpecStructure,
      stats: Stats,
      timer: SimpleTimer,
      options: HtmlOptions,
      arguments: Arguments
  )(ee: ExecutionEnv): Operation[String] =
    makeBody(spec, stats, timer, options, arguments, pandoc = true)(ee).flatMap { body =>
      val variables1 =
        options.templateVariables
          .updated("body", body)
          .updated("title", spec.wordsTitle)
          .updated("path", outputPath(options.outDir, spec).path)

      HtmlTemplate.runTemplate(template, variables1)
    }

  /** WITH PANDOC
    */

  /** Print the execution results as an Html file
    *
    *   - copy resources: css, javascript, template
    *   - create the file content using the template and Pandoc (as an external process)
    */
  def printHtmlWithPandoc(
      env: Env,
      spec: SpecStructure,
      stats: Stats,
      timer: SimpleTimer,
      pandoc: Pandoc
  ): Operation[Unit] =
    import env.{fileSystem as fs}
    for
      options <- getHtmlOptions(env.arguments)
      _ <- fs.withEphemeralFile(options.outDir | options.template.name) {
        (fs.copyFile(options.outDir)(options.template) |||
          logger.warnAndFail("No template file found at " + options.template.path, RunAborted)) >>
          makePandocHtml(spec, stats, timer, pandoc, options, env)
      }
    yield ()

  /** Create the Html file by invoking Pandoc
    */
  def makePandocHtml(
      spec: SpecStructure,
      stats: Stats,
      timer: SimpleTimer,
      pandoc: Pandoc,
      options: HtmlOptions,
      env: Env
  ): Operation[Unit] =
    import env.{fileSystem as fs}

    val variables1 =
      options.templateVariables
        .updated("title", spec.wordsTitle)

    val bodyFile: FilePath =
      options.outDir | FileName.unsafe("body-" + spec.hashCode)

    val outputFilePath = outputPath(options.outDir, spec)
    val pandocArguments = Pandoc.arguments(bodyFile, options.template, variables1, outputFilePath, pandoc)

    fs.withEphemeralFile(bodyFile) {
      makeBody(spec, stats, timer, options, env.arguments, pandoc = true)(env.specs2ExecutionEnv).flatMap { body =>
        fs.writeFile(bodyFile, body) >>
          logger.warn(pandoc.executable.path + " " + pandocArguments.mkString(" ")).when(pandoc.verbose) >>
          Executable.run(pandoc.executable, pandocArguments) >>
          fs.replaceInFile(outputPath(options.outDir, spec), "<code>", "<code class=\"prettyprint\">")
      }
    }

  def copyResources(env: Env, options: HtmlOptions): Operation[List[Unit]] =
    env.fileSystem.mkdirs(options.outDir) >> {
      List(DirectoryPath("css"), DirectoryPath("javascript"), DirectoryPath("images"), DirectoryPath("templates"))
        .map(
          copySpecResourcesDir(env, "org" / "specs2" / "reporter", options.outDir, classOf[HtmlPrinter].getClassLoader)
        )
        .sequence
        .recover { (t: Throwable) =>
          val message = "Cannot copy resources to " + options.outDir.path + "\n" + t.getMessage
          logger.warnAndFail[List[Unit]](message, RunAborted + message)
        }
    }

  def copySpecResourcesDir(env: Env, base: DirectoryPath, outputDir: DirectoryPath, loader: ClassLoader)(
      src: DirectoryPath
  ): Operation[Unit] =
    Option(loader.getResource((base / src).path)) match
      case Some(url) =>
        val fs = env.fileSystem
        if url.getProtocol.equalsIgnoreCase("jar") then
          fs.unjarOnce(jarOf(url), outputDir, s"^${quote(base.path)}(/${quote(src.path)}/.*)$$")
        else fs.copyDir(DirectoryPath.unsafe(url.toURI), outputDir / src)
      case _ =>
        val message = s"no resource found for path ${(base / src).path}"
        logger.warnAndFail(message, message)

  def reportMissingSeeRefs(specs: List[SpecStructure], outDir: DirectoryPath)(using ee: ExecutionEnv): Operation[Unit] =
    for
      missingSeeRefs <- specs
        .flatMap(_.seeReferencesList)
        .distinct
        .filterM(ref => FilePathReader.doesNotExist(SpecHtmlPage.outputPath(outDir, ref.specClassName)))
      _ <- logger
        .warn(
          "The following specifications are being referenced but haven't been reported\n" +
            missingSeeRefs.map(_.specClassName).distinct.mkString("\n")
        )
        .unless(missingSeeRefs.isEmpty)
    yield ()

  private def jarOf(url: URL): URL = url.openConnection.asInstanceOf[JarURLConnection].getJarFileURL

object HtmlPrinter:

  val RunAborted =
    "\nHtml run aborted!\n "

class DefaultHtmlPrinter(env: Env) extends HtmlPrinter(env, SearchPage())
