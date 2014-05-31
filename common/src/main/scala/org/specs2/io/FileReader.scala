package org.specs2
package io

import java.io._
import scala.xml.NodeSeq
import control.Exceptions._
import scala.xml.parsing._
import scala.io.Source
import scala.io.Source._
import xml.Nodex._
import text.MD5


/**
 * The FileReader trait provides functions to read files
 * It can be overridden if necessary to mock that behaviour
 */
private[specs2]
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
   * @return a FileInputStream for a given file path
   */
  def inputStream(filePath: String): java.io.InputStream = new java.io.FileInputStream(filePath)

  /** @return the MD5 hash of a file */
  def md5(f: File): String = MD5.md5Hex(readBytes(f.getPath))

  /** @return the path of a File relative to a base file */
  def fromBaseFile(base: File) = (aFile: File) => Paths.from(base.getPath)(aFile.getPath)


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
private[specs2]
object FileReader extends FileReader