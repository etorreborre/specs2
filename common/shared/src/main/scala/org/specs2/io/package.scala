package org.specs2

import java.io.File
import scala.quoted._
import scala.quoted.matching._

package object io {

  /**
   * It is possible to create a DirPath or FilePath, starting from a string "tmp" </> "dir"
   */
  implicit class NameToDirPathSyntax(name: FileName) {
    def /(other: FileName): DirectoryPath  = DirectoryPath(name) / other
    def |(other: FileName): FilePath = DirectoryPath(name) | other
  }

  /**
   * It is possible to create a FileName from a string provided it is well-formed
   */
  inline implicit def ToFileName(s: String): FileName =
    ${createFileName('{s})}

  /**
   * create a file name from a String
   */
  def fileNameFromString(s: String): Option[FileName] =
    if (s.contains(File.separator) || isWindows && s.contains("/")) None
    else Some(FileName.unsafe(s))

  def createFileName(fileName: Expr[String])(using qctx: QuoteContext): Expr[FileName] =
    fileName match {
      case e@Const(s) =>
        '{fileNameFromString($e) match {
            case None =>
              ${qctx.throwError(s"$fileName is not a valid fileName. It must not contain a /", fileName)}

            case Some(fn) =>
              FileName.unsafe(fn.name)
         }}

      case _ =>
        qctx.throwError(s"$fileName is not a valid fileName. It must not contain a /", fileName)
    }

  val isWindows = sys.props("os.name").startsWith("Windows")

}
