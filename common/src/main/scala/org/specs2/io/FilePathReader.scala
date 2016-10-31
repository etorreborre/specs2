package org.specs2
package io

import java.io.{BufferedInputStream, File, FileInputStream}

import control._
import text.MD5

import scala.io.Codec
import Paths._
import org.specs2.control.producer._
import org.specs2.control.eff.all._

/**
 * Methods to read files on the FileSystem
 */
trait FilePathReader {

  /**
   * @return the list of file paths accessible from dir
   */
  def filePaths(dir: DirectoryPath, glob: String, verbose: Boolean): Operation[List[FilePath]] = {
    filePathsProcess[OperationStack](dir)
      .filter(filterWithPattern(globToPattern(glob)))
      .runList
  }

  /**
   * filter files according to a regex pattern
   */
  def filterWithPattern(pattern: String): FilePath => Boolean = { filePath: FilePath =>
    // remove any letter drive on Windows
    filePath.path.replaceFirst(".:", "").unixize matches pattern
  }

  /**
   * @return the regular expression equivalent to a glob pattern (see the specs for Fragments)
   */
  def globToPattern(glob: String): String = {
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
  def listFilePaths(directory: DirectoryPath): Operation[List[FilePath]] =
    filePathsProcess[OperationStack](directory).runList

  /**
   * @return the files directly accessible from a directory
   */
  def listDirectFilePaths(directory: DirectoryPath): Operation[IndexedSeq[FilePath]] =
    Operations.delayed(Option(directory.toFile.listFiles)
      .map(_.toIndexedSeq).getOrElse(IndexedSeq())
      .filter(_.isFile)
      .map(FilePath.unsafe))

  /**
   * @return the files directly accessible from a directory
   */
  def listDirectDirectoryPaths(directory: DirectoryPath): Operation[IndexedSeq[DirectoryPath]] =
    Operations.delayed(Option(directory.toFile.listFiles)
      .map(_.toIndexedSeq).getOrElse(IndexedSeq())
      .filter(_.isDirectory)
      .map(DirectoryPath.unsafe))

  private def filePathsProcess[R :_safe](directory: DirectoryPath): Producer[R, FilePath] = {
    def go(dir: DirectoryPath): Producer[R, FilePath] = {
      val (files, directories) = Option(dir.toFile.listFiles).map(_.toList).getOrElse(List()).partition(_.isFile)
      Producer.emit(files.map(FilePath.unsafe)) append Producer.emit(directories.map(DirectoryPath.unsafe).map(go)).flatten
    }
    go(directory)
  }

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
    Operations.delayed(scala.io.Source.fromFile(filePath.path)(codec).getLines.toIndexedSeq)

  /** read the content of a file as an Array of Bytes */
  def readBytes(filePath: FilePath): Operation[Array[Byte]] = exists(filePath).map { exists =>
    val stream = new BufferedInputStream(new FileInputStream(filePath.path))
    try     Stream.continually(stream.read).takeWhile(-1 !=).map(_.toByte).toArray
    finally stream.close()
  }

  /** @return the MD5 hash of a file */
  def md5(filePath: FilePath): Operation[String] =
    readBytes(filePath).map(MD5.md5Hex)

  /** @return true if the file exists */
  def exists(filePath: FilePath): Operation[Boolean] =
    Operations.delayed(filePath.toFile.exists)

  /** @return true if the file doesn't exist */
  def doesNotExist(filePath: FilePath): Operation[Boolean] =
    Operations.delayed(!filePath.toFile.exists)

  /** @return true if the directory exists */
  def exists(directoryPath: DirectoryPath): Operation[Boolean] =
    Operations.delayed(directoryPath.toFile.exists)

  /** @return true if the directory doesn't exist */
  def doesNotExist(directoryPath: DirectoryPath): Operation[Boolean] =
    Operations.delayed(!directoryPath.toFile.exists)

  /** succeeds if the file exists */
  def mustExist(file: File): Operation[Unit] =
    Operations.delayed(file.exists).flatMap { exists =>
      if (exists) Operations.ok(())
      else        Operations.fail(s"$file does not exist")
    }

  /** succeeds if the file is a directory */
  def mustBeADirectory(file: File): Operation[Unit] =
    Operations.delayed(file.isDirectory).flatMap { isDirectory =>
      if (isDirectory) Operations.ok(())
      else             Operations.fail(s"$file is a directory")
    }

  /** succeeds if the file is not a directory */
  def mustNotBeADirectory(file: File): Operation[Unit] =
    Operations.delayed(file.isDirectory).flatMap { isDirectory =>
      if (isDirectory) Operations.fail(s"$file is a directory")
      else             Operations.ok(())
    }
}

object FilePathReader extends FilePathReader
