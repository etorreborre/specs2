package org.specs2
package main

import java.io.File

import io.*
import text.*
import text.Split.*
import reflect.*
import scala.reflect.{ClassTag}

/** Command-line arguments
  */
case class CommandLine(_arguments: Seq[String] = Seq()) extends ShowArgs:

  def arguments: Seq[String] = _arguments
  def contains(a: String) = arguments contains a
  def isDefined(name: String) = value(name).isDefined

  /** @return
    *   true if a specific string is present in the list of arguments from the command line or if an attribute with that
    *   name (and any value) has been defined
    */
  def isSet(name: String) = contains(name) || isDefined(name)

  /** @return
    *   the value for a given attribute attribute names and values are defined in a positional way where an attribute
    *   name is always succeeded with an attribute value. For example:
    *
    * name1 value1 name2 value2
    *
    * values can also be retrieved from system properties set with the regular jvm syntax `-Dname=value`
    */
  def value(name: String): Option[String] =
    Arguments.value(name)(using _arguments, SystemProperties)

  /** try to instantiate a class from its name */
  def instance[T <: AnyRef](name: String)(using t: ClassTag[T]): Option[T] =
    Arguments.value(name)(using _arguments, SystemProperties).flatMap(n => Classes.createInstanceFromName(n).runOption)

  def valueOr(name: String, defaultValue: String) = value(name).getOrElse(defaultValue)

  def map(name: String) = value(name).map(vs => Map(vs.split(",").map(v => (v.split("=")(0), v.split("=")(1)))*))
  def mapOr(name: String, defaultValue: Map[String, String]) = map(name).getOrElse(defaultValue)

  def directory(name: String) = value(name).map(n => DirectoryPath.unsafe(new File(n).getAbsolutePath).asAbsolute)
  def directoryOr(name: String, defaultValue: DirectoryPath) = directory(name).getOrElse(defaultValue)

  def file(name: String) = value(name).map(n => FilePath.unsafe(new File(n).getAbsolutePath).asAbsolute)
  def fileOr(name: String, defaultValue: FilePath) = file(name).getOrElse(defaultValue)

  def int(name: String) = Arguments.int(name)(using _arguments, SystemProperties)
  def intOr(name: String, defaultValue: Int) = int(name).getOrElse(defaultValue)

  def long(name: String) = Arguments.long(name)(using _arguments, SystemProperties)
  def longOr(name: String, defaultValue: Long) = long(name).getOrElse(defaultValue)

  def double(name: String) = Arguments.double(name)(using _arguments, SystemProperties)
  def doubleOr(name: String, defaultValue: Double) = double(name).getOrElse(defaultValue)

  def float(name: String) = Arguments.float(name)(using _arguments, SystemProperties)
  def floatOr(name: String, defaultValue: Float) = float(name).getOrElse(defaultValue)

  def bool(name: String) = Arguments.bool(name)(using _arguments, SystemProperties)
  def boolOr(name: String, defaultValue: Boolean) = bool(name).getOrElse(defaultValue)

  def filter(included: String*) = copy(_arguments = arguments.filter(included.toSet.contains))
  def filterNot(excluded: String*) = copy(_arguments = arguments.filterNot(excluded.toSet.contains))
  def overrideWith(other: CommandLine) =
    copy(_arguments = if other.arguments.isEmpty then this._arguments else other.arguments)

  override def toString = _arguments.mkString("CommandLine(", ", ", ")")

object CommandLine extends Extract:
  def create(values: String*): CommandLine =
    new CommandLine(values)

  def extract(using arguments: Seq[String], systemProperties: SystemProperties): CommandLine =
    new CommandLine(_arguments = value("commandline").map(splitValues).getOrElse(Seq()) ++ arguments)

  val allArguments: Seq[ArgumentType] =
    Select.allArguments ++
      Store.allArguments ++
      Execute.allArguments ++
      Report.allArguments ++
      FilesRunnerArguments.allArguments

  // other arguments which are not mentioned in Arguments
  // this is a stop gap measure until a more modular solution is found
  val extraArguments: Seq[ArgumentType] = List(
    BooleanArgument("verbose"),
    BooleanArgument("console"),
    BooleanArgument("all"),
    BooleanArgument("silent"),
    BooleanArgument("pandoc"),
    ValuedArgument("scalacheck.mintestsok"),
    ValuedArgument("scalacheck.minsize"),
    ValuedArgument("scalacheck.maxdiscardratio"),
    ValuedArgument("scalacheck.maxsize"),
    ValuedArgument("scalacheck.workers"),
    ValuedArgument("scalacheck.seed"),
    ValuedArgument("scalacheck.verbosity"),
    ValuedArgument("sbt.tags"),
    ValuedArgument("stats.outdir"),
    ValuedArgument("junit.outdir"),
    ValuedArgument("markdown.outdir"),
    ValuedArgument("markdown.ext"),
    ValuedArgument("html.outdir"),
    ValuedArgument("html.template"),
    ValuedArgument("html.variables"),
    ValuedArgument("html.nostats"),
    ValuedArgument("html.search"),
    ValuedArgument("html.toc"),
    ValuedArgument("html.toc.entrymaxsize"),
    ValuedArgument("html.warn.missingref")
  )

  val allArgumentNames = allArguments.map(_.name)

  def splitValues(arguments: String): Seq[String] =
    splitValues(arguments.split(" ").toIndexedSeq)

  def splitValues(arguments: Seq[String]): Seq[String] =
    arguments.splitDashed(allArgumentNames)

  // try to find if incorrect arguments have been passed on the command line
  def unknownArguments(implicit arguments: Seq[String]): List[String] = {
    arguments.toList match {
      case List() =>
        List()
      case name :: value :: rest =>
        findArgument(name) match {
          case Some(BooleanArgument(_)) =>
            if (FromString[Boolean].fromString(value).isDefined) unknownArguments(rest)
            else unknownArguments(value :: rest)
          case Some(ValuedArgument(_)) =>
            unknownArguments(rest)
          case _ =>
            name :: unknownArguments(value :: rest)
        }
      case name :: _ =>
        findArgument(name) match {
          case Some(_) => List()
          case _       => List(name)
        }
    }
  }

  private def findArgument(name: String): Option[ArgumentType] =
    allArguments.find {
      case BooleanArgument(n) =>
        (name.startsWith("!") && n.toLowerCase == name.drop(1).toLowerCase) ||
          (n.toLowerCase == name.toLowerCase)
      case ValuedArgument(n) =>
        n.toLowerCase == name.toLowerCase
    }

case class FilesRunnerArguments(
    verbose: Boolean,
    basePath: String,
    glob: String,
    pattern: String
)

object FilesRunnerArguments:
  /** base path for the specification files */
  val specificationsBasePath: String =
    "src/test/scala"

  /** glob pattern for the file paths inside the base path */
  val specificationsPath: String =
    "**/*.scala"

  /** Regex pattern used to capture a specification name in an object/class declaration */
  val specificationsPattern: String =
    "(.*Spec)\\s*extends\\s*.*"

  def extract(args: Arguments): FilesRunnerArguments =
    FilesRunnerArguments(
      verbose = args.isSet("filesrunner.verbose"),
      basePath =
        args.commandLine.valueOr("filesrunner.basepath", new java.io.File(specificationsBasePath).getAbsolutePath),
      glob = args.commandLine.valueOr("filesrunner.path", specificationsPath),
      pattern = args.commandLine.valueOr("filesrunner.pattern", specificationsPattern)
    )

  val allArguments: List[ArgumentType] =
    List(
      BooleanArgument("filesrunner.verbose"),
      ValuedArgument("filesrunner.basepath"),
      ValuedArgument("filesrunner.path"),
      ValuedArgument("filesrunner.pattern")
    )
