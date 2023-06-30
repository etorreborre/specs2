package org.specs2

import java.io.File

package object io:

  /** It is possible to create a DirPath or FilePath, starting from a string "tmp" </> "dir"
    */
  extension (name: FileName) def /(other: FileName): DirectoryPath = DirectoryPath(name) / other
  def |(other: FileName): FilePath = DirectoryPath(name) | other
