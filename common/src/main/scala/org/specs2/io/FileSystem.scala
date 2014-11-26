package org.specs2
package io

import control._
import ActionT._
import scalaz.{std, syntax, stream, concurrent}
import std.anyVal._
import std.list._
import syntax.all._
import stream._
import concurrent.Task
import java.io._
import java.util.regex.Pattern._
import java.util.regex.Matcher._
import java.net.URL
import java.util.zip._
import java.util.regex.Pattern.compile
import java.util.regex.Matcher.quoteReplacement
import FileReader._
import org.specs2.text.Regexes._

/**
 * Interface for the FileSystem where effects are denoted with the "Action" type
 */
trait FileSystem extends FilePathReader {

  /** delete a file */
  def deleteFile(filePath: FilePath): Action[Boolean] =
    Actions.safe(filePath.toFile.delete)

  /** write a string to a file as UTF-8 */
  def writeFile(filePath: FilePath, content: String): Action[Unit] =
    Actions.fromTask(writeFileTask(filePath, content))

  /** modify the content of a file */
  def updateFileContent(filePath: FilePath)(update: String => String): Action[Unit] =
    readFile(filePath).flatMap(s => writeFile(filePath, update(s)))

  /** replace a string in a file */
  def replaceInFile(filePath: FilePath, source: String, target: String): Action[Unit] =
    updateFileContent(filePath)(_.replace(source, target))

  /** write a string to a file as UTF-8 */
  def writeFileTask(filePath: FilePath, content: String): Task[Unit] =
    mkdirs(filePath).toTask >>
    Process(content).toSource.pipe(text.utf8Encode).to(io.fileChunkW(filePath.path)).run

  /** execute an action with a File, then delete it */
  def withEphemeralFile(path: FilePath)(action: Action[Unit]): Action[Unit] =
    action.andFinally(deleteFile(path).void)

  /** create a directory and its parent directories */
  def mkdirs(path: DirectoryPath): Action[Unit] =
    Actions.safe(path.toFile.mkdirs)

  /** create a the directory containing a file and its parent directories */
  def mkdirs(path: FilePath): Action[Unit] =
    mkdirs(path.dir)

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
  def unjar(jarUrl: URL, dest: DirectoryPath, regexFilter: String): Action[Unit] = {
    val regex = compile(regexFilter)
    val uis = jarUrl.openStream()
    val zis = new ZipInputStream(new BufferedInputStream(uis))

    @annotation.tailrec
    def extractEntry(entry: ZipEntry) {
      if (entry != null) {
        val matcher = regex.matcher(entry.getName)
        if (matcher.matches) {
          val target = matcher.replaceFirst(s"${quoteReplacement(dest.path)}$$1")
          if (!entry.isDirectory) {
            new File(target).getParentFile.mkdirs
            new File(target).createNewFile
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
  def copyDir(src: DirectoryPath, dest: DirectoryPath): Action[Unit] =
    mkdirs(dest) >>
      listDirectFilePaths(src).flatMap { files =>
        files.toList.map(copyFile(dest)).sequenceU.void
      } >>
      listDirectDirectoryPaths(src).flatMap { directories: IndexedSeq[DirectoryPath] =>
        directories.toList.map(dir => copyDir(dir, dest / dir.name)).sequenceU.void
      }

  /**
   * copy a file to a destination directory
   * @param filePath path of the file to copy
   * @param dest destination directory path
   */
  def copyFile(dest: DirectoryPath)(filePath: FilePath): Action[Unit] = Actions.safe {
    import java.nio.file._
    Files.copy(Paths.get(filePath.path),
               Paths.get(dest.path).resolve(Paths.get(filePath.name.name)), StandardCopyOption.REPLACE_EXISTING)
  }

  /** create a new file */
  def createFile(filePath: FilePath): Action[Boolean] =
    mkdirs(filePath.dir) >>
    Actions.safe(filePath.toFile.createNewFile)

  /** delete files or directories */
  def delete(file: FilePath): Action[Unit] =
    Actions.safe(file.toFile.delete).void

  /** delete a directory */
  def delete(dir: DirectoryPath): Action[Unit] =
    listFilePaths(dir).flatMap(_.map(delete).toList.sequenceU.void) >>
    delete(dir.toFilePath) // delete the directory once it is empty
}

object FileSystem extends FileSystem
