package org.specs2
package io

import java.io._
import java.net.URI
import java.util.UUID
import scalaz._, Scalaz._

/**
 * Representation of a directory relative to some base location
 *
 * It is a list of FileNames and we can append other DirPaths or FilePaths to it
 *
 * If the list is empty, this means we are at the location root
 */
case class DirectoryPath(dirs: Vector[FileName]) {

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
  def path: String = if (isRoot) "" else dirs.map(_.name).toList.mkString("/")

  /** @return the path for this file as a / separated string, with a final / */
  def dirPath: String = dirs.map(_.name).toList.mkString("", "/", "/")

  /** @return a File for this path */
  def toFile: File = new File(path)

  /**
   * append another directory path
   *
   * DirectoryPath.Root plays the role an empty element for this operation
   */
  def </>(other: DirectoryPath): DirectoryPath =
    (this, other) match {
      case (_, DirectoryPath.ROOT) => this
      case (DirectoryPath.ROOT, _) => other
      case _                 => copy(dirs = dirs ++ other.dirs)
    }

  /**
   * append a FilePath to this directory
   * @return another FilePath
   */
  def </>(other: FilePath): FilePath =
    FilePath(DirectoryPath(dirs ++ other.dir.dirs), other.name)

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
      case (h +: t, h1 +: t1) if h == h1 => copy(dirs = t).relativeTo(other.copy(dirs = t1))
      case _                             => this
    }

  /** @return the DirectoryPath starting from the root */
  def fromRoot: DirectoryPath = relativeTo(root)

  /** @return interpret this DirectoryPath as a FilePath, which might be /. if this DirectoryPath is Root */
  def toFilePath: FilePath = FilePath(dir, name)

  def isRoot = dirs.isEmpty

}

object DirectoryPath {
  def apply(n: FileName): DirectoryPath = apply(Vector(n))

  def apply(uuid: UUID): DirectoryPath = apply(FileName(uuid))

  def unsafe(s: String): DirectoryPath =
    DirectoryPath(removeScheme(s).split("/").filter(_.nonEmpty).map(FileName.unsafe).toVector)

  def unsafe(f: File): DirectoryPath = unsafe(f.getPath)
  def unsafe(uri: URI): DirectoryPath = unsafe(uri.toString)

  private def removeScheme(s: String): String = Seq("file:").foldLeft(s) { (res, cur) => res.replace(cur, "") }

  val ROOT = DirectoryPath(dirs = Vector())
}

/**
 * Representation of a file relative to some base location
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

  /** @return return the portion of the file path that starts from the rootname */
  def fromRoot: FilePath = relativeTo(root)

  /** @return interpret this FilePath as a DirectoryPath */
  def toDirectoryPath: DirectoryPath = dir </> name

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
  def </>(other: DirectoryPath) : DirectoryPath  = DirectoryPath(this +: other.dirs)
  def </>(other: FilePath): FilePath = FilePath(DirectoryPath(this +: other.dir.dirs), other.name)
  def </>(other: FileName): DirectoryPath  = DirectoryPath(this) </> other
  def <|>(other: FileName): FilePath = DirectoryPath(this) <|> other
}

object FileName {
  def unsafe(s: String) = new FileName(s)
  def apply(uuid: UUID) = new FileName(uuid.toString)
}



