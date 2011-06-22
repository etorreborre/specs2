package org.specs2
package io

import main.SystemProperties._
import java.io._
import java.nio.charset.Charset

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
    val out = getWriter(path)
    try {
      function(out)
    } finally {
      try { out.close() } 
      catch { case _ => }
    }
  }
  /**
   * creates a file for a given path. Create the parent directory if necessary.
   */
  def createFile(path: String) = {
    if (new File(path).getParentFile != null && !new File(path).getParentFile.exists) 
      mkdirs(new File(path).getParent) 
    if (!new File(path).exists) 
      new File(path).createNewFile
  }

  /** creates a new directory */
  def mkdirs(path: String) = new File(path).mkdirs
  /**
   * writes some content to a file.
   * @param path path of the file to read
   * @param content content of the file to write
   */
  def writeFile(path: String, content: =>String): Unit = write(path) { out => out.write(content) }

  /**
   * The getWriter function can be overriden to provide a mock writer writing to the console for example
   * @return a Writer object opened on the file designated by <code>path</code>
   */
  def getWriter(path: String): Writer =
    new BufferedWriter(new OutputStreamWriter(new FileOutputStream(path), Charset.forName(getOrElse("specs2.file.encoding", "UTF-8"))))
}

private[specs2]
trait MockFileWriter extends FileWriter {
  override def createFile(path: String) = {}
  private val writer = new MockWriter {}
  def getWriter: MockWriter = writer
  override def getWriter(path: String): Writer = writer
}
