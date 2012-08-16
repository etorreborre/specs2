package org.specs2
package io

import main.SystemProperties._
import java.io._
import java.nio.charset.Charset
import scala.xml._

/**
* The FileWriter trait provides functions to write files
* It can be overriden if necessary to mock that behaviour
*/
private[specs2]
trait FileWriter {

  /**
   * writes some content to a file and take care of closing the file.<p>
   * Usage: <code>
   * write("./dir/hello.txt") { out =>
   *   out.write("content")
   * }
   * </code>
   * @param path path of the file to write
   */
  def write(path: String)(function: Writer => Unit): Unit = {
    createFile(path)
    writeToPath(path)(function)
  }
  /**
   * append some content to a file and take care of closing the file.<p>
   * Usage: <code>
   * write("./dir/hello.txt") { out =>
   *   out.write("content")
   * }
   * </code>
   * @param path path of the file to write
   */
  def append(path: String)(function: Writer => Unit): Unit = {
    if (!exists(path)) createFile(path)
    writeToPath(path, append = true)(function)
  }

  private def writeToPath(path: String, append: Boolean = false)(function: Writer => Unit): Unit = {
    val out = getWriter(path, append)
    try {
      function(out)
    } finally {
      try { out.close() }
      catch { case _: Throwable => }
    }
  }
  /**
   * creates a file for a given path. Create the parent directory if necessary.
   */
  def createFile(path: String) = {
    if (new File(path).getParentFile != null && !new File(path).getParentFile.exists) 
      mkdirs(new File(path).getParent) 
    if (!exists(path))
      new File(path).createNewFile
  }
  /** @return true if the file exists */
  def exists(path: String) = path != null && new File(path).exists
  /** creates a new directory */
  def mkdirs(path: String) = new File(path).mkdirs
  /** delete a file */
  def delete(path: String) = new File(path).delete
  /**
   * writes some content to a file.
   * @param path path of the file to read
   * @param content content of the file to write
   */
  def writeFile(path: String, content: =>String): Unit = write(path) { out => out.write(content) }
  /**
   * writes some xml content to a file.
   * @param path path of the file to read
   * @param content content of the file to write
   */
  def writeXmlFile(path: String, content: =>NodeSeq): Unit = writeFile(path, Xhtml.toXhtml(content))
  /**
   * writes some content to a file.
   * @param path path of the file to read
   * @param content content of the file to write
   */
  def appendToFile(path: String, content: =>String): Unit = append(path) { out => out.write(content) }
  /**
   * writes some xml content to a file.
   * @param path path of the file to read
   * @param content content of the file to write
   */
  def appendToXmlFile(path: String, content: =>NodeSeq): Unit = appendToFile(path, Xhtml.toXhtml(content))

  /**
   * The getWriter function can be overriden to provide a mock writer writing to the console for example
   * @return a Writer object opened on the file designated by <code>path</code>
   */
  def getWriter(path: String, append: Boolean = false): Writer =
    new BufferedWriter(new OutputStreamWriter(new FileOutputStream(path, append), Charset.forName(getOrElse("specs2.file.encoding", "UTF-8"))))
}

private[specs2]
object FileWriter extends FileWriter

private[specs2]
trait MockFileWriter extends FileWriter {
  override def createFile(path: String) = {}
  override def delete(path: String) = true
  private val writer = new MockWriter {}
  def getWriter: MockWriter = writer
  override def getWriter(path: String, append: Boolean = false): Writer = writer
}
