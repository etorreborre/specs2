package org.specs2
package reporter

import specification.core._
import data.Fold
import specification.process.{Stats, Statistics}
import io._
import io.Paths._
import main.Arguments
import scala.xml.NodeSeq
import scalaz.concurrent.Task
import scalaz.syntax.bind._
import control._
import java.util.regex.Pattern._
import java.io.File
import java.net.{JarURLConnection, URL}
import scalaz.std.list._
import scalaz.std.anyVal._
import scalaz.syntax.traverse._
import scalaz.syntax.bind._
import html.HtmlTemplate
import scala.sys.process.ProcessLogger
import execute._
import text.NotNullStrings._

trait HtmlPrinter extends Printer {
  def fold(env: Env, spec: SpecStructure): Fold[Fragment] = new Fold[Fragment] {
    type S = Stats

    lazy val sink = Fold.unitSink[Fragment, Stats]

    def prepare = Task.now(())
    def fold = Statistics.fold
    def init = Stats()

    def last(stats: Stats) = {
      val action =
        getPandoc(env).flatMap {
          case None         => printHtml(env, spec, stats)
          case Some(pandoc) => printHtmlWithPandoc(env, spec, stats, pandoc)
        }
      action.toTask
    }
  }

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

  def getHtmlOptions(arguments: Arguments): Action[HtmlOptions] = {
    import arguments.commandLine._
    val out = directoryOr("html.outdir", HtmlOptions.outDir).asAbsolute
    Actions.ok(HtmlOptions(
      outDir    = out,
      baseDir   = directoryOr("html.basedir", HtmlOptions.baseDir),
      template  = fileOr("html.template", HtmlOptions.template(out)),
      variables = mapOr("html.variables", HtmlOptions.variables),
      noStats   = boolOr("html.nostats", HtmlOptions.noStats)))
  }


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

  def printHtmlWithPandoc(env: Env, spec: SpecStructure, stats: Stats, pandoc: Pandoc): Action[Unit] = {
    import env.fileSystem._

    for {
      options  <- getHtmlOptions(env.arguments)
      _        <- copyResources(env, options.outDir)
      _        <- withFile(options.outDir <|> options.template.name) {
                    copyFile(options.outDir)(options.template) >>
                    makePandocHtml(spec, stats, pandoc, options, env)
                  }
    } yield ()
  }

  def makePandocHtml(spec: SpecStructure, stats: Stats, pandoc: Pandoc, options: HtmlOptions, env: Env): Action[Unit] =  {
    import env.fileSystem._

    val variables1 =
      options.variables
        .updated("title", spec.wordsTitle)
        .updated("baseDir", options.baseDir.path)
        .updated("outDir", options.outDir.path)

    val bodyFile: FilePath =
      options.outDir <|> FileName.unsafe("body-"+spec.hashCode)

    val pandocArguments = Pandoc.arguments(bodyFile, options.template, variables1, outputFilePath(options.outDir, spec), pandoc)

    withFile(bodyFile) {
      writeFile(bodyFile, makeBody(spec, stats, options, env.arguments, pandoc = true)) >>
      runProcess(pandoc.executable, pandocArguments)
    }
  }

  def runProcess(executable: FilePath, arguments: Seq[String] = Seq()): Action[Unit] = {
    val logger = new StringProcessLogger
    try {

      val code = sys.process.Process(executable.path, arguments).!(logger)
      if (code == 0) Actions.ok(())
      else           Actions.fail(logger.lines)
    } catch { case t: Throwable =>
      Actions.fail(t.getMessage+"\n"+logger.lines)
    }
  }

  def outputFilePath(directory: DirectoryPath, spec: SpecStructure): FilePath =
    directory <|> FileName.unsafe(spec.specClassName+".html")

  case class HtmlOptions(outDir: DirectoryPath, baseDir: DirectoryPath, template: FilePath, variables: Map[String, String], noStats: Boolean)

  object HtmlOptions {
    val outDir    = "target" </> "specs2-reports"
    val baseDir   = DirectoryPath.EMPTY
    val variables = Map[String, String]()
    val noStats   = false

    def template(outDir: DirectoryPath): FilePath =
      outDir </> "templates" <|> "specs2.html"
  }

  case class Pandoc(executable: FilePath,
                    inputFormat: String,
                    outputFormat: String) {

    def isExecutableAvailable: Action[Unit] =
      runProcess(executable, Seq("--version"))
  }
  
  object Pandoc {
    val executable   = FilePath("pandoc")
    val inputFormat  = "markdown+pipe_tables"
    val outputFormat = "html"
    
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

  }

  def getPandoc(env: Env): Action[Option[Pandoc]] = {
    import env.arguments.commandLine._
    val markdown = boolOr("pandoc", true)

    if (markdown) {
      val pandoc = Pandoc(
        executable   = fileOr("pandoc.exec", Pandoc.executable),
        inputFormat  = valueOr("pandoc.inputformat", Pandoc.inputFormat),
        outputFormat = valueOr("pandoc.outputformat", Pandoc.outputFormat))

      pandoc.isExecutableAvailable.map(_ => Option(pandoc)).orElse(
        Actions.fail[Option[Pandoc]]("the pandoc executable is not available at: "+pandoc.executable.path))
    }

    else Actions.ok(None)
  }

  def copyResources(env: Env, outDir: DirectoryPath): Action[List[Unit]] =
    env.fileSystem.mkdirs(outDir) >>
    List(DirectoryPath("css"),
         DirectoryPath("javascript"),
         DirectoryPath("images"),
         DirectoryPath("templates")).
      map(copySpecResourcesDir(env, "org" </> "specs2" </> "reporter", outDir, classOf[HtmlPrinter].getClassLoader)).sequenceU


  def makeBody(spec: SpecStructure, stats: Stats, options: HtmlOptions, arguments: Arguments, pandoc: Boolean): String = {
    val title = spec.name

    s"""${spec.fragments.fragments.map(printFragment(arguments, options.outDir, pandoc)).mkString("")}""" ++
    s"""${printStatistics(title, stats, options)}"""
  }

  def printFragment(arguments: Arguments, baseDir: DirectoryPath, pandoc: Boolean) = (fragment: Fragment) => {

    fragment match {
      case t if Fragment.isText(t) =>
        val text = t.description.show

        if (text.trim.nonEmpty) {
          if (pandoc) scala.xml.Unparsed(text)
          else {
            // remove additional newlines and replace with just one when there is no markdown formatting
            val brStart = if (text.filterNot(_ == ' ').startsWith("\n")) <br/> else NodeSeq.Empty
            val brEnd   = if (text.filterNot(_ == ' ').endsWith("\n"))   <br/> else NodeSeq.Empty

            <text class="ok">{brStart}{scala.xml.Unparsed(text.trim)}{brEnd}</text>
          }
        } else NodeSeq.Empty

      case e if Fragment.isExample(e) =>
        e.executionResult match {
          case r: Success =>
            <li class="example success ok">{show(e)}</li>

          case f1 @ Failure(m, e1, st, details) =>
            failureElement("example", f1, show(e), m, arguments.failtrace, arguments)

          case er @ Error(m, e1) =>
            errorElement("example", er, show(e), m, arguments)

          case r: Skipped =>
            <li class="example skipped ok">{show(e)}<br/>
              <message class="skipped">{r.message}</message>
            </li>

          case r: Pending =>
            <li class="example pending ok">{show(e)}<br/>
              <message class="pending">{r.message}</message>
            </li>

          case r =>
            <li class="example info ok">{show(e)}<br/>
              <message class="info">{r.message}</message>
            </li>
        }

      case f if Fragment.isStepOrAction(f) =>
        f.executionResult match {
          case f1 @ Failure(m, e1, st, details) =>
            failureElement("step", f1, <message class="failure">Failed step!</message>, m, arguments.failtrace, arguments)

          case er @ Error(m, e1) =>
            errorElement("step", er, <message class="error">Error in a step!</message>, m, arguments)

          case other => NodeSeq.Empty
        }

      case Fragment(link: SpecificationLink,_,_) =>
        <link class="ok"><a href={link.url.rebase(baseDir.path)} tooltip={link.tooltip} class="ok">{link.linkText}</a></link>

      case Fragment(form @ FormDescription(_),_,_) =>
        form.xml(arguments)

      case other => NodeSeq.Empty
    }
  }

  def toggleElement(a: Any) = "toggleImage(this); showHide('"+id(a)+"')"
  def id(a: Any) = System.identityHashCode(a).toString

  def show(f: Fragment) =
    f.description.show

  def showStacktrace(id: String, st: List[StackTraceElement], klass: String, arguments: Arguments) =
    <stacktrace id={id} style="display:none" class={klass}>
      { arguments.traceFilter(st).map(t => <stacktrace-elt>{t.toString.replace("$", ".")}<br/></stacktrace-elt>).foldLeft(NodeSeq.Empty)(_ ++ _) }
    </stacktrace>

  def failureElement(element: String, f: Result with ResultStackTrace, description: Any, m: String, showTrace: Boolean, arguments: Arguments) = {
    val message = <message class="failure">{m.notNull+" ("+f.location(arguments.traceFilter)+")"}</message>
    val fullMessage =
      if (showTrace) <li class ="failure toggle" onclick={toggleElement(f)}>{message}</li>
      else           <li class ="failure notoggle">{message}</li>

    val trace =
      if (showTrace) showStacktrace(id(f), f.stackTrace, "failure", arguments)
      else NodeSeq.Empty

    <li class={s"$element failure"}>{description}<br/>
      {fullMessage}
      {trace}</li>
  }

  def errorElement(element: String, er: Result with ResultStackTrace, description: Any, m: String, arguments: Arguments) = {
    <li class={s"$element error"}>{description}<br/>
      <li class ="error toggle" onclick={toggleElement(er)}>
        <message class="error">{m.notNull+" ("+er.location(arguments.traceFilter)+")"}</message>
      </li>
      {showStacktrace(id(er), er.stackTrace, "error", arguments)}
    </li>
  }

  def printStatistics(title: String, stats: Stats, options: HtmlOptions) =
    if (options.noStats) ""
    else {
      val statsClass = if (stats.hasErrors) "error" else if (stats.hasIssues) "failure" else "success"
      
      <table class="datatable">
        <tr><th colSpan="2">{s"Total for specification ${title.trim}"}</th></tr>
        <tr><td>Finished in</td><td class="info">{stats.time}</td></tr>
        <tr><td>Results</td><td class={statsClass}>
          {stats.displayResults(Arguments("nocolor"))}</td></tr>
      </table>
    }

  def copySpecResourcesDir(env: Env, base: DirectoryPath, outputDir: DirectoryPath, loader: ClassLoader)(src: DirectoryPath): Action[Unit] = {
    Option(loader.getResource((base </> src).path)) match {
      case None =>
        Actions.fail(s"no resource found for url ${(base </> src).path}")

      case Some(url) =>
        val fs = env.fileSystem
        if (url.getProtocol.equalsIgnoreCase("jar"))
          fs.unjar(jarOf(url), outputDir, s"^${quote(base.path)}(/${quote(src.path)}/.*)$$")
        else
          fs.copyDir(DirectoryPath.unsafe(url.toURI), outputDir </> src)
    }
  }

  val NullProcessLogger = new ProcessLogger {
    def buffer[T](f: => T): T = f
    def err(s: => String) {}
    def out(s: => String) {}
  }

  def stringProcessLogger = new StringProcessLogger
  class StringProcessLogger extends ProcessLogger {
    private val messages = new StringBuilder
    def lines = messages.toString

    def buffer[T](f: => T): T = {
      messages.clear
      f
    }
    def err(s: => String) { messages.append(s+"\n") }
    def out(s: => String) { messages.append(s+"\n") }
  }

  private def jarOf(url: URL): URL = url.openConnection.asInstanceOf[JarURLConnection].getJarFileURL

}

object HtmlPrinter extends HtmlPrinter
