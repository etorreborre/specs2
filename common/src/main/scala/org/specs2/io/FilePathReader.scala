package org.specs2
package io

import control._
import scalaz.std.anyVal._

trait FilePathReader {
  /**
   * @return all the files in a directory according to a glob pattern
   */
  def filePaths(dir: DirectoryPath, glob: String): Action[Seq[FilePath]] =
    Actions.safe(FileReader.filePaths(dir.path, glob).map(FilePath.unsafe))

  def readLines(filePath: FilePath): IndexedSeq[String] = {
    if (exists(filePath)) scala.io.Source.fromFile(filePath.toFile ).getLines.toIndexedSeq
    else                  IndexedSeq()
  }

  /** @return true if the file exists */
  def exists(filePath: FilePath) = filePath.toFile.exists

  /** @return true if the director exists */
  def exists(directoryPath: DirectoryPath) = directoryPath.toFile.exists

}

object FilePathReader extends FilePathReader
