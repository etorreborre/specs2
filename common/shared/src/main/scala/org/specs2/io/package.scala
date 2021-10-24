package org.specs2

import java.io.File

package object io {

  /**
   * It is possible to create a DirPath or FilePath, starting from a string "tmp" </> "dir"
   */
  implicit class NameToDirPathSyntax(name: FileName) {
    def /(other: FileName): DirectoryPath  = DirectoryPath(name) / other
    def |(other: FileName): FilePath = DirectoryPath(name) | other
  }

  /**
   * create a file name from a String
   */
  def fileNameFromString(s: String): Option[FileName] =
    if (s.contains(File.separator) || isWindows && s.contains("/")) None
    else Some(FileName.unsafe(s))

  val isWindows = sys.props("os.name").startsWith("Windows")
}
