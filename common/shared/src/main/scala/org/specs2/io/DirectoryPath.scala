package org.specs2
package io

import java.io.*
import java.io.File.separator
import java.net.URI
import java.util.UUID
import FileName.*

/** Representation of a directory path which can be relative or absolute
  *
  * It is a list of FileNames and we can append other DirectoryPaths or FilePaths to it If the list is empty, this means
  * we are at the root
  */
case class DirectoryPath(dirs: Vector[FileName], absolute: Boolean, separator: String = File.separator)
    derives CanEqual:

  /** @return either the parent directory or the root if we already are at the root */
  def dir: DirectoryPath = parent.getOrElse(this)

  /** @return the last file name of the list or . if the list is empty */
  def name: FileName = dirs.lastOption.getOrElse(FileName.unsafe("."))

  /** @return the dir path for the first name in the list */
  def root: DirectoryPath = copy(dirs = dirs.take(1))

  /** @return the parent directory for this directory, none if we are at the root */
  def parent: Option[DirectoryPath] =
    dirs match
      case h +: tail => Some(copy(dirs = dirs.dropRight(1)))
      case _         => None

  /** @return the path for this file as a / separated string */
  def path: String = (if absolute then separator else "") + dirs.map(_.name).toList.mkString(separator)

  /** @return the path for this file as a / separated string, with a final / */
  def dirPath: String = if isRoot then path else path + separator

  /** @return a File for this path */
  def toFile: File = new File(path)

  /** append another directory path
    *
    * DirectoryPath.Root plays the role an empty element for this operation
    */
  def /(other: DirectoryPath): DirectoryPath =
    (this, other) match
      case (_, DirectoryPath.EMPTY) => this
      case (DirectoryPath.EMPTY, _) => other
      case _                        => copy(dirs = dirs ++ other.dirs)

  /** append a FilePath to this directory
    * @return
    *   another FilePath
    */
  def /(other: FilePath): FilePath =
    FilePath(DirectoryPath(dirs ++ other.dir.dirs, absolute), other.name)

  /** append a new name to this directory
    * @return
    *   a DirectoryPath
    */
  def /(name: FileName): DirectoryPath = copy(dirs = dirs :+ name)

  /** append a new name to this directory but
    * @return
    *   a FilePath
    */
  def |(name: FileName): FilePath = FilePath(this, name)

  /** @return
    *   the portion of a dir path that is relative to another
    */
  def relativeTo(other: DirectoryPath): DirectoryPath =
    if dirs.take(other.dirs.size) == other.dirs then copy(dirs = dirs.drop(other.dirs.size), absolute = false)
    else this

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

object DirectoryPath:
  def apply(n: FileName): DirectoryPath = DirectoryPath(Vector(n), absolute = false)

  def apply(uuid: UUID): DirectoryPath = apply(FileName(uuid))

  /** The separator parameter is there to support tests */
  def unsafe(s: String, separator: String = File.separator): DirectoryPath =
    val withoutScheme = removeScheme(if separator == "\\" then s.replaceAll("\\\\", "/") else s)
    val isAbsolute = withoutScheme.startsWith("/") || isWindows && new File(withoutScheme).isAbsolute
    DirectoryPath(
      withoutScheme.split("/").filter(_.nonEmpty).map(FileName.unsafe).toVector,
      isAbsolute,
      separator
    )

  def unsafe(f: File): DirectoryPath = unsafe(f.getPath)
  def unsafe(uri: URI): DirectoryPath = unsafe(uri.toString)

  private def removeScheme(s: String): String = Seq("file:").foldLeft(s) { (res, cur) => res.replace(cur, "") }

  val Root = DirectoryPath(dirs = Vector(), absolute = true)
  val EMPTY = DirectoryPath(dirs = Vector(), absolute = false)
