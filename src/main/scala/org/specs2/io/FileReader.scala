package org.specs2
package io

import java.io._
import scala.xml.NodeSeq
import control.Exceptions._
import scala.xml.parsing._
import scala.io.Source._
import xml.Nodex._
/**
 * The FileReader trait provides functions to read files
 * It can be overridden if necessary to mock that behaviour
 */
private[specs2]
trait FileReader {
  def readLines(path: String) = scala.io.Source.fromFile(path).getLines.toIndexedSeq
  /**
   * reads the content of a file
   * @param path the path of the file to read
   * @return the content of the file at path
   */
  def readFile(path: String): String = {
    if (!new File(path).exists)
      ""
    else {
      def appendLines(result: StringBuffer, in: BufferedReader, line: String): Unit = {
        if (line != null) {
          result.append(line)
          result.append("\n")
          appendLines(result, in, in.readLine)
        }
      }
      val in = new BufferedReader(new java.io.FileReader(path));
      try {
        val result = new StringBuffer
        appendLines(result, in, in.readLine)
        result.toString
      } finally { in.close() }
    }
  }
  /**
   * @return a FileInputStream for a given file path
   */
  def inputStream(filePath: String): java.io.InputStream = new java.io.FileInputStream(filePath)

  /**
   * @return the xml content of a file
   */
  def loadXmlFile(filePath: String) = tryo(scala.xml.XML.load(filePath))(e => e.printStackTrace).getOrElse(NodeSeq.Empty)

  /**
   * @return the xml content of a file
   */
  def loadXhtmlFile(filePath: String) = tryo {
    val fileContent = readFile(filePath)
    val xhtml = fromString("<e>"+fileContent+"</e>")
    val result = (XhtmlParser(xhtml)\\"e")(0).child.reduceNodes
    result
  }(e => e.printStackTrace).getOrElse(NodeSeq.Empty)
}
private[specs2]
object FileReader extends FileReader