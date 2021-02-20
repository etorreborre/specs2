package org.specs2
package main

import java.io.File

import io._
import text.Split._

/**
 * Command-line arguments
 */
case class CommandLine(_arguments: Seq[String] = Seq()) extends ShowArgs:

  def arguments: Seq[String] = _arguments
  def contains(a: String) = arguments contains a
  def isDefined(name: String) = value(name).isDefined

  /**
   * @return true if a specific string is present in the list of arguments from the command line
   *         or if an attribute with that name (and any value) has been defined
   */
  def isSet(name: String) = contains(name) || isDefined(name)

  /**
   * @return the value for a given attribute
   *         attribute names and values are defined in a positional way where an attribute name is always succeeded
   *         with an attribute value. For example:
   *
   *         name1 value1 name2 value2
   *
   * values can also be retrieved from system properties set with the regular jvm syntax `-Dname=value`
   */
  def value(name: String): Option[String] =
    Arguments.value(name)(using _arguments, SystemProperties)

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
  def overrideWith(other: CommandLine) = copy(_arguments = if other.arguments.isEmpty then this._arguments else other.arguments)

  override def toString = _arguments.mkString("CommandLine(", ", ", ")")

object CommandLine extends Extract:
  def create(values: String*): CommandLine =
    new CommandLine(values)

  def extract(using arguments: Seq[String], systemProperties: SystemProperties): CommandLine =
    new CommandLine(_arguments = value("commandline").map(splitValues).getOrElse(Seq()) ++ arguments)

  val allValueNames = Select.allValueNames ++ Store.allValueNames ++ Execute.allValueNames ++ Report.allValueNames

  def splitValues(arguments: String): Seq[String] =
    splitValues(arguments.split(" ").toIndexedSeq)

  def splitValues(arguments: Seq[String]): Seq[String] =
    arguments.splitDashed(allValueNames)
