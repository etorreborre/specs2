package org.specs2
package io

import java.io._
import java.net.{JarURLConnection, URL}
import java.util.zip._
import Paths._
import java.util.regex.Pattern.{quote, compile}
import java.util.regex.Matcher.quoteReplacement

/**
 * The FileSystem trait abstracts file system operations to allow easier mocking of file system related functionalities.
 * <p>
 * It mixes the `FileReader` and `FileWriter` traits to provide easy read/write operations.
 */
private[specs2]
trait FileSystem extends org.specs2.io.FileReader with org.specs2.io.FileWriter {
  /**
   * @param path glob expression, for example: `./dir/**/*.xml`
   * @return the list of paths represented by the "glob" definition `path`
   */
  def filePaths(basePath: String = ".", path: String = "*", verbose: Boolean = false): Seq[String] = {
    val found = recurse(new File(basePath))
    if (verbose) found.foreach { f => println("found file: "+f) }
    filterFiles(found, path, verbose)
  }

  def filterFiles(found: Seq[File], path: String, verbose: Boolean): Seq[String] = {
    val pattern = globToPattern(path) + (if (isDir(path)) "/*.*" else "")
    if (verbose) println("\nThe pattern used to match files is: "+pattern)
    found.collect { case f if fileMatchesPattern(f, pattern, verbose) => f.getPath }.toSeq
  }

  private def isVersionFile(f: File) = Seq(".svn", ".cvs").exists(f.getPath.contains(_))

  private def fileMatchesPattern(f: File, pattern: String, verbose: Boolean = false) = {
    val filePath = f.getPath
      .replaceFirst(".:", "") // remove any letter drive on Windows
      .unixize

    if (verbose) println(filePath+" matches pattern: "+(filePath matches pattern))
    filePath matches pattern
  }

  /**
   * @param file start file
   * @return a Stream with all the recursively accessible files (not directories)
   */
  private def recurse(file: File): Stream[File] = {
    import Stream._
    cons(file, if (file.listFiles == null) empty else file.listFiles.toStream.filterNot(isVersionFile).flatMap(recurse)).filter(_.isFile)
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
   * @return true if the File represented by this path is a directory
   */
  def isDir(path: String) = isDirectory(path)

  /**
   * creates a directory for a given path
   */
  def createDir(path: String) = (new File(path)).mkdirs

  /**
   * deletes the directory and all directory content at the specified path and return the parent path of that directory
   */
  def removeDir(path: String): String = {
    val dir = new File(path)
    if (dir.isDirectory) { 
      if (dir.listFiles == null || dir.listFiles.isEmpty)
        dir.delete
      else {
        dir.listFiles.foreach { file => 
          if (file.isFile) 
            file.delete
          else 
            removeDir(file.getPath)
        }
        dir.delete
      }
    }
    dir.getParent
  }
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
   * copy the content of a directory to another.
   * @param path path of the directory to copy
   * @param dest destination directory path
   */
  def copyDir(src: String, dest: String) {
    listFiles(src).filterNot(_.contains(".svn")).foreach { name =>
      val path = src + "/" + name
      if (new File(path).isDirectory) copyDir(path, dest) else copyFile(path, dest)
    }
  }
  /** 
   * copy a file to a destination directory
   * @param path path of the file to copy
   * @param dest destination directory path
   */
  def copyFile(path: String, dest: String) {
    mkdirs(dest)
    val destFilePath = dest + "/" + new File(path).getName
    new File(destFilePath).createNewFile
    val input = new BufferedInputStream(new FileInputStream(path))
    try {
      val output  = new BufferedOutputStream(new FileOutputStream(destFilePath), 2048)
      try {
        copy(input, output)
        output.flush
      } finally output.close
    } finally input.close
  }  
  
  /** 
   * Unjar the jar (or zip file) specified by "path" to the "dest" directory.
   * Filters files which shouldn't be extracted with a regular expression.
   * @param path path of the jar file
   * @param dest destination directory path
   * @param regexFilter regular expression filtering files which shouldn't be
   *                    extracted; the expression must capture the path of
   *                    an entry as group 1 which will then be used relative
   *                    to dirPath as target path for that entry
   */
  private def unjar(jarUrl: URL, dirPath: String, regexFilter: String) {
    mkdirs(dirPath)
    val regex = compile(regexFilter)
    val uis = jarUrl.openStream()
    val zis = new ZipInputStream(new BufferedInputStream(uis))

    @annotation.tailrec
    def extractEntry(entry: ZipEntry) {
      if (entry != null) {
        val matcher = regex.matcher(entry.getName)
        if (matcher.matches) {
          val target = matcher.replaceFirst(s"${quoteReplacement(dirPath)}$$1")
          if (entry.isDirectory()) {
            createDir(target)
          } else {
            createFile(target)
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

    try extractEntry(zis.getNextEntry)
    finally zis.close
  }
  
  /** 
   * Copy an input stream to an output stream.
   * @param input input stream
   * @param output output stream
   */
  def copy(input: InputStream, output: OutputStream) {
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
   * Copy specs resources found either in the specs jar or in the classpath directories to an output directory
   *
   * @param base path of the base package (must not be empty!)
   * @param src name of the resource directory to copy (relative to the base package)
   * @param outputDir output directory where to copy the files to
   * @param origin a class that determines the location of the resources
   */
  def copySpecResourcesDir(base: String, src: String, outputDir: String, loader: ClassLoader): Unit =
    Option(loader.getResource(s"$base/$src")) foreach { url =>
      if (url.getProtocol.equalsIgnoreCase("jar")) unjar(jarOf(url), outputDir, s"^${quote(base)}(/${quote(src)}/.*)$$")
      else copyDir(url.getPath, new File(outputDir, src).getPath)
    }

  private def jarOf(url: URL): URL = url.openConnection.asInstanceOf[JarURLConnection].getJarFileURL

  /** @return true if 2 paths are the same according to their canonical representation */
  def samePath(p1: String, p2: String) = new File(p1).getCanonicalPath == new File(p2).getCanonicalPath
}
/**
 * The fs object offers a simple interface to the file system (see the description of the FileSystem trait)
 */
private[specs2]
object fs extends FileSystem

