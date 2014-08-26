package org.specs2
package io

import java.io._
import java.net.URI
import java.util.UUID
import scalaz._, Scalaz._

/**
 * Representation of a directory path which can be relative or absolute
 *
 * It is a list of FileNames and we can append other DirectoryPaths or FilePaths to it
 * If the list is empty, this means we are at the root
 */
case class DirectoryPath(dirs: Vector[FileName], absolute: Boolean) {

  /** @return either the parent directory or the root if we already are at the root */
  def dir: DirectoryPath  = parent.getOrElse(this)

  /** @return the last file name of the list or . if the list is empty */
  def name: FileName = dirs.lastOption.getOrElse(FileName.unsafe("."))

  /** @return the dir path for the first name in the list */
  def root: DirectoryPath = copy(dirs = dirs.take(1))

  /** @return the parent directory for this directory, none if we are at the root */
  def parent: Option[DirectoryPath] =
    dirs match {
      case h +: tail => Some(copy(dirs = dirs.dropRight(1)))
      case _         => None
    }

  /** @return the path for this file as a / separated string */
  def path: String = (if (absolute) "/" else "") + dirs.map(_.name).toList.mkString("/")

  /** @return the path for this file as a / separated string, with a final / */
  def dirPath: String = if (isRoot) path else path+"/"

  /** @return a File for this path */
  def toFile: File = new File(path)

  /**
   * append another directory path
   *
   * DirectoryPath.Root plays the role an empty element for this operation
   */
  def </>(other: DirectoryPath): DirectoryPath =
    (this, other) match {
      case (_, DirectoryPath.EMPTY) => this
      case (DirectoryPath.EMPTY, _) => other
      case _                        => copy(dirs = dirs ++ other.dirs)
    }

  /**
   * append a FilePath to this directory
   * @return another FilePath
   */
  def </>(other: FilePath): FilePath =
    FilePath(DirectoryPath(dirs ++ other.dir.dirs, absolute), other.name)

  /**
   * append a new name to this directory
   * @return a DirectoryPath
   */
  def </>(name: FileName): DirectoryPath  = copy(dirs = dirs :+ name)

  /**
   * append a new name to this directory but
   * @return a FilePath
   */
  def <|>(name: FileName): FilePath = FilePath(this, name)

  /**
   * @return the portion of a dir path that is relative to another
   */
  def relativeTo(other: DirectoryPath): DirectoryPath =
    (dirs, other.dirs) match {
      case (h +: t, h1 +: t1) if h == h1 => copy(dirs = t, absolute = false).relativeTo(other.copy(dirs = t1))
      case _                             => this
    }

  /** @return the DirectoryPath starting from the root */
  def fromRoot: DirectoryPath = relativeTo(root)

  /** @return interpret this DirectoryPath as a FilePath, which might be /. if this DirectoryPath is Root */
  def toFilePath: FilePath = FilePath(dir, name)

  def isRoot = dirs.isEmpty

  /** @return an absolute directory path */
  def asAbsolute = setAbsolute(true)

  /** @return a relative directory path */
  def asRelative = setAbsolute(false)

  /** @return modify the absolute status of this dir path */
  def setAbsolute(isAbsolute: Boolean) = copy(absolute = isAbsolute)

  /** @return true if this directory path is relative */
  def isRelative = !isAbsolute

  /** @return true if this directory path is absolute */
  def isAbsolute = absolute
}

object DirectoryPath {
  def apply(n: FileName): DirectoryPath = DirectoryPath(Vector(n), absolute = false)

  def apply(uuid: UUID): DirectoryPath = apply(FileName(uuid))

  def unsafe(s: String): DirectoryPath = {
    val withoutScheme = removeScheme(s)
    val isAbsolute = withoutScheme.startsWith("/")
    DirectoryPath(withoutScheme.split("/").filter(_.nonEmpty).map(FileName.unsafe).toVector, isAbsolute)
  }

  def unsafe(f: File): DirectoryPath = unsafe(f.getPath)
  def unsafe(uri: URI): DirectoryPath = unsafe(uri.toString)

  private def removeScheme(s: String): String = Seq("file:").foldLeft(s) { (res, cur) => res.replace(cur, "") }

  val ROOT = DirectoryPath(dirs = Vector(), absolute = true)
  val EMPTY = DirectoryPath(dirs = Vector(), absolute = false)
}

/**
 * Representation of a file path, absolute or relative
 *
 * It has a parent directory and a name
 */
case class FilePath(dir: DirectoryPath, name: FileName) {

  /** @return the root directory containing this file */
  def root: DirectoryPath = dir.root

  /** @return the path for this file as a / separated string */
  def path: String = if (dir.isRoot) name.name else dir.path+"/"+name.name

  /** @return a File for this path */
  def toFile: File = new File(path)

  /** @return the portion of a file path that is relative to another */
  def relativeTo(other: DirectoryPath): FilePath =
    copy(dir = dir.relativeTo(other))

  /** @return a file path with the same name but in another directory path */
  def rebaseTo(other: DirectoryPath): FilePath =
    copy(dir = other)

  /** @return return the portion of the file path that starts from the rootname */
  def fromRoot: FilePath = relativeTo(root)

  /** @return interpret this FilePath as a DirectoryPath */
  def toDirectoryPath: DirectoryPath = dir </> name

  /** @return true if the file path is absolute */
  def isAbsolute = dir.isAbsolute

  /** @return an absolute file path */
  def asAbsolute = setAbsolute(absolute = true)

  /** @return a relative file path */
  def asRelative = setAbsolute(absolute = false)

  /** @return modify the absolute status of this file path */
  def setAbsolute(absolute: Boolean) = copy(dir.setAbsolute(absolute))

  /** @return true if this file path is relative */
  def isRelative = !isAbsolute

}

object FilePath {
  def apply(n: FileName): FilePath = FilePath(DirectoryPath.ROOT, n)
  def apply(uuid: UUID): FilePath = apply(FileName(uuid))

  def unsafe(s: String): FilePath = DirectoryPath.unsafe(s).toFilePath
  def unsafe(f: File): FilePath   = DirectoryPath.unsafe(f).toFilePath
  def unsafe(uri: URI): FilePath  = DirectoryPath.unsafe(uri).toFilePath
}

/**
 * The component of a path name according to the unix definition
 *   http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_267
 */
case class FileName private(name: String) {
  def </>(other: DirectoryPath) : DirectoryPath  = DirectoryPath(this +: other.dirs, absolute = false)
  def </>(other: FilePath): FilePath = FilePath(DirectoryPath(this +: other.dir.dirs, absolute = false), other.name)
  def </>(other: FileName): DirectoryPath  = DirectoryPath(Vector(this), absolute = false) </> other
  def <|>(other: FileName): FilePath = DirectoryPath(Vector(this), absolute = false) <|> other
}

object FileName {
  def unsafe(s: String) = new FileName(s)
  def apply(uuid: UUID) = new FileName(uuid.toString)
}



