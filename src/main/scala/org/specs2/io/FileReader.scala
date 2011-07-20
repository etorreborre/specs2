package org.specs2
package io

import java.io._

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
    def appendLines(result: StringBuffer, in: BufferedReader, line: String): Unit = {
      if (line != null) {
        result.append(line)
        result.append("\n")
        appendLines(result, in, in.readLine)
      }
    }
    val in = new BufferedReader(new java.io.FileReader(path));
    val result = new StringBuffer
    appendLines(result, in, in.readLine)
    in.close();
    result.toString
  }
  /**
   * @return a FileInputStream for a given file path
   */
  def inputStream(filePath: String): java.io.InputStream = new java.io.FileInputStream(filePath)

  /**
   * @return the xml content of a file
   */
  def loadXhtmlFile(filePath: String) = scala.xml.parsing.XhtmlParser(scala.io.Source.fromFile(filePath))
}
private[specs2]
object FileReader extends FileReader