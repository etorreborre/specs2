package org.specs2
package io

import java.io._
import scala.xml.parsing._
import scala.io.Source
import text.MD5


/**
 * The FileReader trait provides functions to read files as unsafe functions (not in IO)
 */
trait FileReader {

  /** @return the lines of a file */
  def readLines(path: String): IndexedSeq[String] = {
    if (hasContents(path)) scala.io.Source.fromFile(path).getLines.toIndexedSeq
    else                   IndexedSeq()
  }

  /** @return the bytes of a file */
  def readBytes(path: String): Array[Byte] = {
    if (hasContents(path)) {
      val stream = new BufferedInputStream(new FileInputStream(path))
      try     Stream.continually(stream.read).takeWhile(-1 !=).map(_.toByte).toArray
      finally stream.close
    } else Array[Byte]()
  }

  /**
   * reads the content of a file
   * @param path the path of the file to read
   * @return the content of the file at path
   */
  def readFile(path: String): String = {
    if (hasContents(path)) {
      def appendLines(result: StringBuffer, in: BufferedReader, line: String): Unit = {
        if (line != null) {
          result.append(line)
          result.append("\n")
          appendLines(result, in, in.readLine)
        }
      }
      val in = new BufferedReader(new java.io.FileReader(path))
      try {
        val result = new StringBuffer
        appendLines(result, in, in.readLine)
        result.toString
      } finally in.close
    } else ""
  }

  /** @return true if this path is an existing file which is not a directory */
  private def hasContents(path: String) = new File(path).exists && !new File(path).isDirectory

  /**
   * @return true if the File represented by this path is a directory
   */
  def isDir(path: String) = isDirectory(path)

  private def fileMatchesPattern(f: File, pattern: String, verbose: Boolean = false) = {
    val filePath = "./"+f.getPath.replace("\\", "/")
    if (verbose && f.isFile) println(filePath+" matches pattern: "+(filePath matches pattern))
    f.isFile && (filePath matches pattern)
  }


  /**
   * @return a FileInputStream for a given file path
   */
  def inputStream(filePath: String): java.io.InputStream = new java.io.FileInputStream(filePath)

  /** @return the MD5 hash of a file */
  def md5(f: File): String = MD5.md5Hex(readBytes(f.getPath))

  /** @return the path of a File relative to a base file */
  def fromBaseFile(base: File) = (aFile: File) => Paths.from(base.getPath)(aFile.getPath)

  /** @return true if the file exists */
  def exists(path: String) = path != null && new File(path).exists

  /** @return true if the file can be read */
  def canRead(path: String) = path != null && new File(path).canRead

  /** @return true if the file can be written */
  def canWrite(path: String) = path != null && new File(path).canWrite

  /** @return true if the file is absolute */
  def isAbsolute(path: String) = path != null && new File(path).isAbsolute

  /** @return true if the file is a file */
  def isFile(path: String) = path != null && new File(path).isFile

  /** @return true if the file is a directory */
  def isDirectory(path: String) = path != null && new File(path).isDirectory

  /** @return true if the file is hidden */
  def isHidden(path: String) = path != null && new File(path).isHidden

  /** @return the file name */
  def getName(path: String) = new File(path).getName

  /** @return the file absolute path */
  def getAbsolutePath(path: String) = new File(path).getAbsolutePath

  /** @return the file canonical path */
  def getCanonicalPath(path: String) = new File(path).getCanonicalPath

  /** @return the file parent path */
  def getParent(path: String) = new File(path).getParent

  /** @return the files of that directory */
  def listFiles(path: String): List[String] = if (new File(path).list == null) List() else new File(path).list.toList

  /**
   * @param path glob expression, for example: `./dir/**/*.xml`
   * @return the list of paths represented by the "glob" definition `path`
   */
  def filePaths(basePath: String = ".", path: String = "*", verbose: Boolean = false): Seq[String] = {
    val found = recurse(new File(basePath))
    if (verbose) found.foreach { f => println("found file: "+f) }
    val pattern = globToPattern(path) + (if (isDir(path)) "/*.*" else "")
    if (verbose) println("\nThe pattern used to match files is: "+pattern)
    val collected = found.collect { case f if fileMatchesPattern(f, pattern, verbose) => f.getPath }.toSeq
    collected
  }

  /**
   * @return the regular expression equivalent to a glob pattern
   */
  def globToPattern(glob: String): String = {
    val star = "<STAR>"
    val authorizedNamePattern = "[^\\/\\?<>\\|\\" + star + ":\"]" + star
    val pattern = glob.replace("\\", "/")
      .replace(".", "\\.")
      .replace("**/", "(" + authorizedNamePattern + "/)" + star)
      .replace("*", authorizedNamePattern)
      .replace(star, "*")

    if (!pattern.startsWith("\\./")) "\\./" + pattern
    else                             pattern
  }

  /**
   * @param file start file
   * @return a Stream with all the recursively accessible files
   */
  private def recurse(file: File): Stream[File] = {
    import Stream._
    cons(file, if (file.listFiles == null) empty else file.listFiles.toStream.flatMap(recurse))
  }

  /**
   * @return the xml content of a file using the Xhtml parser
   *
   * if the file contains several nodes, it wraps them up in a single artificial node
   */
  def loadXhtmlFile(filePath: String, sourceErrors: Boolean = true) =
    ???
//  tryo {
//    val fileContent = readFile(filePath)
//    val xhtml = fromString("<e>"+fileContent+"</e>")
//    (parse(xhtml, sourceErrors)\\"e")(0).child.reduceNodes
//  }

  private[this] def parse(source: Source, sourceErrors: Boolean = true) = {
    if (sourceErrors) XhtmlParser(source)
    else new XhtmlParser(source) {
      override def reportSyntaxError(pos: Int, str: String): Unit = ()
    }.initialize.document
  }

//  def silentLoadXhtmlFileReport          = (e: Exception, filePath: String) => ()
//  private[this] def defaultLoadXhtmlFileReport = (e: Exception, filePath: String) => { scala.Console.println("trying to load: "+filePath+"\n"); e.printStackTrace }

}
object FileReader extends FileReader