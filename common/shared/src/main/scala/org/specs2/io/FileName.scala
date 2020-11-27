package org.specs2
package io

import java.io._
import java.net.URI
import java.util.UUID
import scala.quoted._

/**
 * The component of a path name according to the unix definition
 *   http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_267
 */
case class FileName private(name: String):
  def /(other: DirectoryPath) : DirectoryPath  = DirectoryPath(this +: other.dirs, absolute = false)
  def /(other: FilePath): FilePath = FilePath(DirectoryPath(this +: other.dir.dirs, absolute = false), other.name)
  def /(other: FileName): DirectoryPath  = DirectoryPath(Vector(this), absolute = false) / other
  def |(other: FileName): FilePath = DirectoryPath(Vector(this), absolute = false) | other

object FileName:
  def unsafe(s: String) = new FileName(s)
  def apply(uuid: UUID) = new FileName(uuid.toString)

    /**
   * It is possible to create a FileName from a string provided it is well-formed
   */
  implicit inline def ToFileName(s: String): FileName =
    ${createFileName('{s})}

  /**
   * create a file name from a String
   */
  def fileNameFromString(s: String): Either[String, FileName] =
    if s.contains(File.separator) || isWindows && s.contains("/") then
      Left(s"$s is not a valid file name. It must not contain a /")
    else
      Right(FileName.unsafe(s))

  def createFileName(fileName: Expr[String])(using Quotes): Expr[FileName] =
    fileName match
      case e@Const(s) =>
        FileName.fileNameFromString(s) match
          case Left(m) =>
            report.throwError(m, fileName)
          case Right(fn) =>
            '{FileName.unsafe($e)}

      case other =>
        report.throwError(s"Not a valid file name. It must be a literal string without any /", fileName)

  val isWindows = sys.props("os.name").startsWith("Windows")
