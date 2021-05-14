package org.specs2
package io

import java.io.*
import java.net.URI
import java.util.UUID

/**
 * Representation of a file path, absolute or relative
 *
 * It has a parent directory and a name
 */
case class FilePath(dir: DirectoryPath, name: FileName):

  /** @return the root directory containing this file */
  def root: DirectoryPath = dir.root

  /** @return the path for this file as a / separated string */
  def path: String = if dir.isRoot then name.name else dir.path + "/" + name.name

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
  def toDirectoryPath: DirectoryPath = dir / name

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


object FilePath:
  def apply(n: FileName): FilePath = FilePath(DirectoryPath.Root, n)
  def apply(uuid: UUID): FilePath = apply(FileName(uuid))

  def unsafe(s: String): FilePath = DirectoryPath.unsafe(s).toFilePath
  def unsafe(f: File): FilePath   = DirectoryPath.unsafe(f).toFilePath
  def unsafe(uri: URI): FilePath  = DirectoryPath.unsafe(uri).toFilePath
