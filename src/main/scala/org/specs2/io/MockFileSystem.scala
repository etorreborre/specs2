package org.specs2
package io
import scala.collection.mutable. { ListBuffer, Queue, HashMap }
import control.Exceptions._
import scala.xml.NodeSeq
import scala.io.Source._
import scala.xml.parsing.XhtmlParser
import xml.Nodex._
/**
 * The MockFileSystem trait mocks the FileSystem by storing a Map[path, content] representing the content of the FileSystem
 */
trait MockFileSystem extends FileSystem {

  /** default extension which can be used when creating default file names */
  var defaultExtension = ""

  /** this map associates some file paths with file contents */
  var files = new HashMap[String, String]

  /** this list registers created directories*/
  var createdDirs = List[String]()

  /** this map associates some file paths with children paths */
  var children = new HashMap[String, ListBuffer[String]]

  /** this list stores readable files */
  var readableFiles = List[String]()

  /** this list stores writable files */
  var writableFiles = List[String]()

  /** @return the content of a file corresponding to a given path */
  override def readFile(path: String) = files(path)

  /** @return all file paths */
  override def filePaths(basePath: String = ".", path: String = "*", verbose: Boolean = false) = files.keySet.toList

  /** adds a new file to the FileSystem. The file path will be a default one */
  def addFile(content: String): Unit = addFile(defaultFilePath, content)

  /** adds a new file to the FileSystem with a specific file path */
  def addFile(path: String, content: String) = {
    files += Pair(path, content)
    readableFiles ::= path
    writableFiles ::= path
  }
  /** adds a new child to a given file */
  def addChild(parent: String, child: String): Unit = {
    children.get(parent) match {
      case Some(l) => () 
      case None => children.put(parent, new ListBuffer)
    }
    children.get(parent).get += child
  }
  /** sets a file as readable */
  def setReadable(path: String) = {
    if (!canRead(path)) (readableFiles ::= path)
    path
  }
  /** sets a file as writable */
  def setWritable(path: String) = {
    if (!canWrite(path)) (writableFiles ::= path)
    path
  }
  /** sets a file as not readable */
  def setNotReadable(path: String) = {
    readableFiles = readableFiles.filterNot(_ == path)
    path
  }
  /** sets a file as not writable */
  def setNotWritable(path: String) = {
    writableFiles = writableFiles.filterNot(_ == path)
    path
  }
  /** overrides the canRead definition checking in the readableFiles list */
  override def canRead(path: String) = readableFiles.exists(_ == path)
  /** overrides the canWrite definition checking in the writableFiles list */
  override def canWrite(path: String) = writableFiles.exists(_ == path)
  /** overrides the isAbsolute definition checking if it starts with / (partial definition) */
  override def isAbsolute(path: String) = path.startsWith("/") || path.startsWith("\\")
  /** overrides the isHidden definition checking if it starts with . (partial definition) */
  override def isHidden(path: String) = path.startsWith(".")
  /** overrides the isFile definition checking if it doesn't end with / (partial definition) */
  override def isFile(path: String) = path.matches(".*\\..*")
  /** overrides the isDirectory definition checking if it ends with / (partial definition) */
  override def isDirectory(path: String) = !isFile(path)
  /** overrides the listFiles definition */
  override def listFiles(path: String) = children.get(path.replaceAll("\\\\", "/")).map(_.toList).getOrElse(Nil)
  /** @return a default file path. All default file paths will be different from each other */
  def defaultFilePath = "name" + files.size + defaultExtension

  /** creates a file with the specified path but an empty content */
  override def createFile(path: String) = {files += (path -> ""); true}

  /** delete a file with the specified path */
  override def delete(path: String) = {files -= path; true}

  /** create a new directory */
  override def mkdirs(path: String) = { createdDirs = path :: createdDirs; true }
  /** create a new directory */
  override def createDir(path: String) = mkdirs(path)

  /** @return a mock FileWriter for a specific path */
  override def getWriter(path: String, append: Boolean = false) = MockFileWriter(path)

  case class MockFileWriter(path: String) extends MockWriter {
    override def write(m: String): Unit = files(path) = files.getOrElse(path, "") + m
  }
  /** removes all specified files */
  def reset: this.type = {
    files = new HashMap[String, String]
    children = new HashMap[String, ListBuffer[String]]
    readableFiles = Nil
    writableFiles = Nil
    this
  }
  
  override def exists(path: String) = files.contains(path)
  
  override def inputStream(filePath: String) = new java.io.InputStream {
    val reader = new java.io.StringReader(readFile(filePath))
    def read() = reader.read()
  }

  override def loadXmlFile(filePath: String)(report: Exception => Unit = (e:Exception) => e.printStackTrace) = {
    tryo {
      val xhtml = fromString("<e>"+readFile(filePath)+"</e>")
      (XhtmlParser(xhtml)\\"e")(0).child.reduceNodes
    }.getOrElse(NodeSeq.Empty)
  }

}
