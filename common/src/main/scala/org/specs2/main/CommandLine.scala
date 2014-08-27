package org.specs2
package main

import io._
import text.Split._

/**
 * Command-line arguments
 */
case class CommandLine(_arguments: Seq[String] = Seq()) extends ShowArgs {

  def arguments: Seq[String] = _arguments
  def contains(a: String) = arguments contains a
  def isDefined(name: String) = value(name).isDefined

  def value(name: String) = Arguments.value(name)(_arguments, SystemProperties)
  def valueOr(name: String, defaultValue: String) = value(name).getOrElse(defaultValue)

  def map(name: String) = value(name).map(vs => Map(vs.split(",").map(v => (v.split("=")(0), v.split("=")(1))): _*))
  def mapOr(name: String, defaultValue: Map[String, String]) = map(name).getOrElse(defaultValue)

  def directory(name: String) = value(name).map(DirectoryPath.unsafe)
  def directoryOr(name: String, defaultValue: DirectoryPath) = directory(name).getOrElse(defaultValue)

  def file(name: String) = value(name).map(FilePath.unsafe)
  def fileOr(name: String, defaultValue: FilePath) = file(name).getOrElse(defaultValue)

  def int(name: String) = Arguments.int(name)(_arguments, SystemProperties)
  def intOr(name: String, defaultValue: Int) = int(name).getOrElse(defaultValue)

  def bool(name: String) = Arguments.bool(name)(_arguments, SystemProperties)
  def boolOr(name: String, defaultValue: Boolean) = bool(name).getOrElse(defaultValue)

  def filter(included: String*) = copy(_arguments = arguments.filter(included.toSet.contains))
  def filterNot(excluded: String*) = copy(_arguments = arguments.filterNot(excluded.toSet.contains))
  def overrideWith(other: CommandLine) = copy(_arguments = if (other.arguments.isEmpty) this._arguments else other.arguments)

  override def toString = _arguments.mkString("CommandLine(", ", ", ")")
}

object CommandLine extends Extract {
  def extract(implicit arguments: Seq[String], systemProperties: SystemProperties): CommandLine =
    new CommandLine(_arguments = value("commandline").map(splitValues).getOrElse(Seq()) ++ arguments)

  val allValueNames = Select.allValueNames ++ Store.allValueNames ++ Execute.allValueNames ++ Report.allValueNames

  def splitValues(arguments: String): Seq[String] = arguments.splitDashed(allValueNames)
}


