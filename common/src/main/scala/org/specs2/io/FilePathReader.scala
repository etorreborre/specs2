package org.specs2
package io

import java.io.{File, FileInputStream, BufferedInputStream}
import control._
import text.MD5
import scalaz.concurrent._
import scalaz.stream._
import scalaz.std.anyVal._
import Paths._

trait FilePathReader {

  /**
   * @return the list of file paths accessible from dir
   */
  def filePaths(dir: DirectoryPath, glob: String, verbose: Boolean): Action[IndexedSeq[FilePath]] = {
    listFilePaths(dir)
      .filter(filterFiles(globToPattern(glob)))
      .runLog.toAction
  }

  /**
   * filter files according to a regex pattern
   */
  private def filterFiles(pattern: String): FilePath => Boolean = { filePath: FilePath =>
    // remove any letter drive on Windows
    filePath.path.replaceFirst(".:", "").unixize matches pattern
  }

  /**
   * @return the regular expression equivalent to a glob pattern (see the specs for Fragments)
   */
  private def globToPattern(glob: String): String = {
    val star = "<STAR>"
    val authorizedNamePattern = "[^\\/\\?<>\\|\\" + star + ":\"]" + star
    glob.replace("\\", "/")
      .replace(".", "\\.")
      .replace("**/", "(" + authorizedNamePattern + "/)" + star)
      .replace("*", authorizedNamePattern)
      .replace(star, "*")
  }

  /**
   * @return the files accessible recursively from a directory
   */
  private def listFilePaths(directory: DirectoryPath): Process[Task, FilePath] = {
    def go(dir: DirectoryPath): Process[Task, FilePath] = {
      val (files, directories) = Option(dir.toFile.listFiles).map(_.toList).getOrElse(Nil).partition(_.isFile)
      Process.emitAll(files.map(FilePath.unsafe)) fby Process.emitAll(directories.map(DirectoryPath.unsafe).map(go)).flatMap(identity)
    }
    go(directory)
  }

  /** @return the lines of a file */
  def readLines(filePath: FilePath): Action[IndexedSeq[String]] = exists(filePath).map { exists =>
    if (exists) scala.io.Source.fromFile(filePath.toFile ).getLines().toIndexedSeq
    else        IndexedSeq()
  }

  /** read the content of a file as an Array of Bytes */
  def readBytes(filePath: FilePath): Action[Array[Byte]] = exists(filePath).map { exists =>
    val stream = new BufferedInputStream(new FileInputStream(filePath.path))
    try     Stream.continually(stream.read).takeWhile(-1 !=).map(_.toByte).toArray
    finally stream.close()
  }

  /** @return the MD5 hash of a file */
  def md5(filePath: FilePath): Action[String] =
    readBytes(filePath).map(MD5.md5Hex)

  /** @return true if the file exists */
  def exists(filePath: FilePath): Action[Boolean] =
    Actions.safe(filePath.toFile.exists)

  /** @return true if the directory exists */
  def exists(directoryPath: DirectoryPath): Action[Boolean] =
    Actions.safe(directoryPath.toFile.exists)

  /** succeeds if the file is a directory */
  def mustBeADirectory(file: File): Action[Unit] =
    Actions.safe(file.isDirectory).flatMap { isDirectory =>
      if (isDirectory) Actions.fail(s"$file is a directory")
      else             Actions.ok(())
    }

  /** succeeds if the file is not a directory */
  def mustNotBeADirectory(file: File): Action[Unit] =
    Actions.safe(file.isDirectory).flatMap { isDirectory =>
      if (isDirectory) Actions.ok(())
      else             Actions.fail(s"$file is a directory")
    }
}

object FilePathReader extends FilePathReader
