package org.specs2
package io

import java.io.{BufferedInputStream, File, FileInputStream}

import fp.*, syntax.*
import control.*
import producer.*
import text.MD5

import scala.io.Codec
import Paths.*

/** Methods to read files on the FileSystem
  */
trait FilePathReader:

  /** @return
    *   the list of file paths accessible from dir
    */
  def filePaths(dir: DirectoryPath, glob: String, verbose: Boolean): Operation[List[FilePath]] =
    filePathsProcess(dir).filter(filterWithPattern(globToPattern(glob))).runList

  /** filter files according to a regex pattern
    */
  def filterWithPattern(pattern: String): FilePath => Boolean = { (filePath: FilePath) =>
    // remove any letter drive on Windows
    filePath.path.replaceFirst(".:", "").unixize `matches` pattern
  }

  /** @return
    *   the regular expression equivalent to a glob pattern (see the specs for Fragments)
    */
  def globToPattern(glob: String): String =
    val star = "<STAR>"
    val authorizedNamePattern = "[^\\/\\?<>\\|\\" + star + ":\"]" + star
    glob
      .replace("\\", "/")
      .replace(".", "\\.")
      .replace("**/", "(" + authorizedNamePattern + "/)" + star)
      .replace("*", authorizedNamePattern)
      .replace(star, "*")

  /** @return
    *   the files accessible recursively from a directory
    */
  def listFilePaths(directory: DirectoryPath): Operation[List[FilePath]] =
    filePathsProcess(directory).runList

  /** @return
    *   the files directly accessible from a directory
    */
  def listDirectFilePaths(directory: DirectoryPath): Operation[IndexedSeq[FilePath]] =
    Operation.delayed(
      Option(directory.toFile.listFiles)
        .map(_.toIndexedSeq)
        .getOrElse(IndexedSeq())
        .filter(_.isFile)
        .map(FilePath.unsafe)
    )

  /** @return
    *   the files directly accessible from a directory
    */
  def listDirectDirectoryPaths(directory: DirectoryPath): Operation[IndexedSeq[DirectoryPath]] =
    Operation.delayed(
      Option(directory.toFile.listFiles)
        .map(_.toIndexedSeq)
        .getOrElse(IndexedSeq())
        .filter(_.isDirectory)
        .map(DirectoryPath.unsafe)
    )

  private def filePathsProcess(directory: DirectoryPath): Producer[Operation, FilePath] =
    def go(dir: DirectoryPath): Producer[Operation, FilePath] =
      val (files, directories) = Option(dir.toFile.listFiles).map(_.toList).getOrElse(List()).partition(_.isFile)
      Producer.emitSync(files.map(FilePath.unsafe)) `append`
        Producer.emitSync(directories.map(DirectoryPath.unsafe).map(go)).flatten
    go(directory)

  /** @return the content of a file encoded as UTF8 */
  def readFile(path: FilePath): Operation[String] =
    readLines(path).map(_.mkString("\n"))

  /** @return the content of a file as UTF-8 lines */
  def readLines(filePath: FilePath): Operation[IndexedSeq[String]] =
    readLinesWithCodec(filePath, Codec.UTF8)

  /** @return the content of a file with a specific codec */
  def readFileWithCodec(path: FilePath, codec: Codec): Operation[String] =
    readLinesWithCodec(path, codec).map(_.mkString("\n"))

  /** @return the content of a file with a specific codec */
  def readLinesWithCodec(filePath: FilePath, codec: Codec): Operation[IndexedSeq[String]] =
    Operation.delayed(scala.io.Source.fromFile(filePath.path)(codec).getLines.toIndexedSeq)

  /** read the content of a file as an Array of Bytes */
  def readBytes(filePath: FilePath): Operation[Array[Byte]] = exists(filePath).map { exists =>
    val stream = new BufferedInputStream(new FileInputStream(filePath.path))
    try LazyList.continually(stream.read).takeWhile(b => -1 != b).map(_.toByte).toArray
    finally stream.close()
  }

  /** @return the MD5 hash of a file */
  def md5(filePath: FilePath): Operation[String] =
    readBytes(filePath).map(MD5.md5Hex)

  /** @return true if the file exists */
  def exists(filePath: FilePath): Operation[Boolean] =
    Operation.delayed(filePath.toFile.exists)

  /** @return true if the file doesn't exist */
  def doesNotExist(filePath: FilePath): Operation[Boolean] =
    Operation.delayed(!filePath.toFile.exists)

  /** @return true if the directory exists */
  def exists(directoryPath: DirectoryPath): Operation[Boolean] =
    Operation.delayed(directoryPath.toFile.exists)

  /** @return true if the directory doesn't exist */
  def doesNotExist(directoryPath: DirectoryPath): Operation[Boolean] =
    Operation.delayed(!directoryPath.toFile.exists)

  /** succeeds if the file exists */
  def mustExist(file: File): Operation[Unit] =
    Operation.delayed(file.exists).flatMap { exists =>
      if exists then Operation.ok(())
      else Operation.fail(s"$file does not exist")
    }

  /** succeeds if the file is a directory */
  def mustBeADirectory(file: File): Operation[Unit] =
    Operation.delayed(file.isDirectory).flatMap { isDirectory =>
      if isDirectory then Operation.ok(())
      else Operation.fail(s"$file is a directory")
    }

  /** succeeds if the file is not a directory */
  def mustNotBeADirectory(file: File): Operation[Unit] =
    Operation.delayed(file.isDirectory).flatMap { isDirectory =>
      if isDirectory then Operation.fail(s"$file is a directory")
      else Operation.ok(())
    }

object FilePathReader extends FilePathReader
