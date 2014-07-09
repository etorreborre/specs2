package org.specs2
package reporter

import org.specs2.collection.Seqx
import org.specs2.execute.Failure
import org.specs2.execute.Pending
import org.specs2.execute.Skipped
import org.specs2.specification.core.SpecificationLink
import org.specs2.specification.core._
import data.Fold
import specification.process.{Stats, Statistics}
import io.Paths._
import main.Arguments
import scala.Some
import scala.xml.NodeSeq
import scalaz.concurrent.Task
import scalaz.syntax.bind._
import io._
import control._
import java.util.regex.Pattern._
import java.io.File
import java.net.{JarURLConnection, URL}
import scalaz.std.list._
import scalaz.std.anyVal._
import scalaz.syntax.traverse._
import scalaz.syntax.bind._
import Actions._
import html.HtmlTemplate
import text.Trim._
import scala.sys.process.ProcessLogger
import execute._
import text.NotNullStrings._
import io.Paths.toPath
import Seqx._

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
    val out = value("html.outdir").getOrElse(HtmlOptions.outDir).absoluteDirPath
    Actions.ok(HtmlOptions(
      outDir    = out,
      baseDir   = value("html.basedir").getOrElse(HtmlOptions.baseDir),
      template  = value("html.template").getOrElse(HtmlOptions.template(out)),
      variables = value("html.variables").fold(HtmlOptions.variables)(vs => Map(vs.split(",").map(v => (v.split("=")(0), v.split("=")(1))): _*)),
      noStats   = bool("html.nostats").getOrElse(HtmlOptions.noStats)))
  }


  def makeHtml(template: String, spec: SpecStructure, stats: Stats, options: HtmlOptions, arguments: Arguments): Action[String] = {
    val body = makeBody(spec, stats, options, arguments, pandoc = true)
    val variables1 =
      options.variables
        .updated("body", body)
        .updated("title", spec.name)
        .updated("baseDir", options.baseDir)
        .updated("outDir", options.outDir)
    HtmlTemplate.runTemplate(template, variables1)
  }

  def printHtmlWithPandoc(env: Env, spec: SpecStructure, stats: Stats, pandoc: Pandoc): Action[Unit] = {
    import env.fileSystem._

    for {
      options  <- getHtmlOptions(env.arguments)
      _        <- copyResources(env, options.outDir)
      _        <- withFile(options.outDir+options.template.fileName) {
                    copyFile(options.template, options.outDir) >>
                    makePandocHtml(spec, stats, pandoc, options, env)
                  }
    } yield ()
  }

  def makePandocHtml(spec: SpecStructure, stats: Stats, pandoc: Pandoc, options: HtmlOptions, env: Env): Action[Unit] =  {
    import env.fileSystem._

    val variables1 =
      options.variables
        .updated("title", spec.name)
        .updated("baseDir", options.baseDir)
        .updated("outDir", options.outDir)

    val bodyFile = options.outDir+"body-"+spec.hashCode
    val pandocArguments = Pandoc.arguments(bodyFile, options.template, variables1, outputFilePath(options.outDir, spec), pandoc)

    withFile(bodyFile) {
      writeFile(bodyFile, makeBody(spec, stats, options, env.arguments, pandoc = true)) >>
      runProcess(pandoc.executable, pandocArguments)
    }
  }

  def runProcess(executable: String, arguments: Seq[String] = Seq()): Action[Unit] = {
    val logger = new StringProcessLogger
    try {

      val code = sys.process.Process(executable, arguments).!(logger)
      if (code == 0) Actions.ok(())
      else           Actions.fail(logger.lines)
    } catch { case t: Throwable =>
      Actions.fail(t.getMessage+"\n"+logger.lines)
    }
  }

  def outputFilePath(directory: String, spec: SpecStructure) =
    directory+spec.specClassName+".html"

  case class HtmlOptions(outDir: String, baseDir: String, template: String, variables: Map[String, String], noStats: Boolean)

  object HtmlOptions {
    val outDir = "target/specs2-reports/"
    def template(outDir: String) = outDir+"/templates/specs2.html"
    val variables = Map[String, String]()
    val baseDir = "."
    val noStats = false
  }

  case class Pandoc(executable: String,
                    inputFormat: String,
                    outputFormat: String) {
    def isExecutableAvailable: Action[Unit] =
      runProcess(s"$executable", Seq("--version"))
  }
  
  object Pandoc {
    val executable = "pandoc"
    val inputFormat = "markdown+pipe_tables"
    val outputFormat = "html"
    
    def arguments(bodyPath: String, templatePath: String, variables: Map[String, String], outputFile: String, options: Pandoc): Seq[String] = {
      val variablesOption = variables.flatMap { case (k, v) => Seq("-V", s"$k=$v") }

      Seq(bodyPath,
          "-f", options.inputFormat,
          "-t", options.outputFormat,
          "--template", templatePath,
          "-s", "-S",
          "-o", outputFile) ++
      variablesOption
    } 

  }

  def getPandoc(env: Env): Action[Option[Pandoc]] = {
    import env.arguments.commandLine._
    val markdown = bool("pandoc").getOrElse(true)

    if (markdown) {
      val pandoc = Pandoc(
        executable   = value("pandoc.exec")       .getOrElse(Pandoc.executable),
        inputFormat  = value("pandoc.inputformat").getOrElse(Pandoc.inputFormat),
        outputFormat = value("pandoc.outputformat").getOrElse(Pandoc.outputFormat))

      pandoc.isExecutableAvailable.map(_ => Some(pandoc)).orElse(
        Actions.fail("the pandoc executable is not available at: "+pandoc.executable))
    }

    else Actions.ok(None)
  }

  def copyResources(env: Env, outDir: String): Action[List[Unit]] =
    env.fileSystem.mkdirs(outDir) >>
    List("css", "javascript", "images", "templates").
      map(copySpecResourcesDir(env, "org/specs2/reporter", outDir, classOf[HtmlPrinter].getClassLoader)).sequenceU


  def makeBody(spec: SpecStructure, stats: Stats, options: HtmlOptions, arguments: Arguments, pandoc: Boolean): String = {
    val title = spec.name

    s"""${spec.fragments.fragments.map(printFragment(arguments, options.outDir, pandoc)).mkString("\n")}""" ++
    s"""${printStatistics(title, stats, options)}"""
  }

  def printFragment(arguments: Arguments, baseDir: String, pandoc: Boolean) = (fragment: Fragment) => {

    fragment match {
      case t if Fragment.isText(t) =>
        val text = t.description.show

        if (text.trim.nonEmpty) {
          if (pandoc) <t>{scala.xml.Unparsed(text)}</t>
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
        <link class="ok"><a href={link.url.rebase(baseDir)} tooltip={link.tooltip} class="ok">{link.linkText}</a></link>

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

  def copySpecResourcesDir(env: Env, base: String, outputDir: String, loader: ClassLoader)(src: String): Action[Unit] = {
    Option(loader.getResource(s"$base/$src")) match {
      case None =>
        Actions.fail(s"no resource found for url $base/$src")

      case Some(url) =>
        val fs = env.fileSystem
        if (url.getProtocol.equalsIgnoreCase("jar"))
          fs.unjar(jarOf(url), outputDir, s"^${quote(base)}(/${quote(src)}/.*)$$")
        else
          fs.copyDir(url.getPath, new File(outputDir, src).getPath)
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
