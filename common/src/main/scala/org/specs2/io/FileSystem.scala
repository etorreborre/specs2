package org.specs2
package io

import control._
import Actions._
import data.Processes
import scalaz.{std, syntax, stream, concurrent}
import std.anyVal._
import syntax.bind._
import std.list._
import syntax.traverse._
import stream._
import concurrent.Task
import java.io._
import data.Processes
import Processes._
import java.net.URL
import java.util.regex.Pattern._
import java.util.zip.{ZipEntry, ZipInputStream}
import java.util.regex.Matcher._
import FileReader._

/**
 * Interface for the FileSystem where effects are denoted with the "Action" type
 */
trait FileSystem {
  /**
   * @param path glob expression, for example: `./dir/**/*.xml`
   * @return the list of paths represented by the "glob" definition `path`
   */
  def filePaths(basePath: String, path: String, verbose: Boolean): Action[Seq[String]] = for {
    found <- listFiles(basePath)
    _     <- found.toList.map(f => log("found file: "+f, verbose)).sequenceU
    pattern = globToPattern(path) + (if (isDirectory(path)) "/*.*" else "")
    _     <- log("\nThe pattern used to match files is: "+pattern, verbose)
  } yield found.collect { case f if fileMatchesPattern(f, pattern) => f.getPath }.toSeq

  /** @return true if the file path matches the pattern */
  private def fileMatchesPattern(f: File, pattern: String) = {
    val filePath = "./"+f.getPath.replace("\\", "/")
    f.isFile && (filePath matches pattern)
  }

  private def listFiles(path: String): Action[IndexedSeq[File]] =
    Actions.fromTask(listFiles(new File(path)).runLog[Task, File])

  private def listFiles(file: File): Process[Task, File] = {
    def go(f: File): Process[Task, File] =
      Process.await(Task.delay(f)) { file =>
        val children = Option(file.listFiles).map(_.toList).getOrElse(Nil)

        if (children.isEmpty) Process.halt
        else                  Process.emitSeq(children, children.map(go).sequenceU.flatten)
      }
    go(file)
  }

  /** @return true if the file is a directory */
  def isDirectory(path: String) = path != null && new File(path).isDirectory

  def readFile(path: String): Action[String] =
    readLines(path).map(_.mkString("\n"))

  def readLines(path: String): Action[Seq[String]] =
    Actions.fromTask(io.linesR(path).runLog[Task, String])

  def deleteFile(path: String): Action[Boolean] =
    Actions.safe(new File(path).delete)

  def writeFile(path: String, content: String): Action[Unit] =
    Actions.fromTask(writeFileTask(path, content))

  def writeFileTask(path: String, content: String): Task[Unit] =
    Process(content).toSource.pipe(process1.utf8Encode).to(io.fileChunkW(path)).run

  def withFile(path: String)(action: Action[Unit]): Action[Unit] =
    (action >> deleteFile(path)).orElse(deleteFile(path)).map(_ => ())


  def mkdirs(path: String): Action[Unit] = Actions.safe(new File(path).mkdirs)

  /**
   * Unjar the jar (or zip file) specified by "path" to the "dest" directory.
   * Filters files which shouldn't be extracted with a regular expression.
   * @param jarUrl path of the jar file
   * @param dest destination directory path
   * @param regexFilter regular expression filtering files which shouldn't be
   *                    extracted; the expression must capture the path of
   *                    an entry as group 1 which will then be used relative
   *                    to dirPath as target path for that entry
   */
  def unjar(jarUrl: URL, dest: String, regexFilter: String): Action[Unit] = {
    val regex = compile(regexFilter)
    val uis = jarUrl.openStream()
    val zis = new ZipInputStream(new BufferedInputStream(uis))

    @annotation.tailrec
    def extractEntry(entry: ZipEntry) {
      if (entry != null) {
        val matcher = regex.matcher(entry.getName)
        if (matcher.matches) {
          val target = matcher.replaceFirst(s"${quoteReplacement(dest)}$$1")
          if (entry.isDirectory) {
            mkdirs(target)
          } else {
            mkdirs(target)
            val fos = new FileOutputStream(target)
            val dest = new BufferedOutputStream(fos, 2048)

            try {
              copy(zis, dest)
              dest.flush
            } finally dest.close
          }

        }
        extractEntry(zis.getNextEntry)
      }
    }

    Actions.safe {
      try     extractEntry(zis.getNextEntry)
      finally zis.close
    }
  }

  /**
   * Copy an input stream to an output stream.
   * @param input input stream
   * @param output output stream
   */
  private def copy(input: InputStream, output: OutputStream) {
    val data = new Array[Byte](2048)
    def readData(count: Int): Unit = {
      if (count != -1) {
        output.write(data, 0, count)
        output.flush
        readData(input.read(data, 0, 2048))
      }
    }
    readData(input.read(data, 0, 2048))
  }

  /**
   * copy the content of a directory to another.
   * @param src path of the directory to copy
   * @param dest destination directory path
   */
  def copyDir(src: String, dest: String): Action[Unit] =
    mkdirs(dest) >>
      listFiles(src).flatMap { files =>
        files.toList.map { file =>
          if (file.isDirectory) copyDir(file.getPath, dest)
          else                  copyFile(file.getPath, dest)
        }.sequenceU.map(_ => ())
      }

  /**
   * copy a file to a destination directory
   * @param path path of the file to copy
   * @param dest destination directory path
   */
  def copyFile(path: String, dest: String): Action[Unit] = Actions.safe {
    import java.nio.file._
    Files.copy(Paths.get(path), Paths.get(dest).resolve(Paths.get(new File(path).getName)), StandardCopyOption.REPLACE_EXISTING)
  }

  /** create a new file */
  def createFile(path: String): Action[Boolean] =
    Actions.safe { new File(path).createNewFile }

  /** delete files or directories */
  def delete(path: String): Action[Unit] =
    listFiles(path).map(_.reverse.map(_.delete)).map(_ => ())
}

object FileSystem extends FileSystem
