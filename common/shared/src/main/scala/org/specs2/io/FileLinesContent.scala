package org.specs2
package io

import java.io.File
import text._
import control._

/**
 * Default implementation for reading lines out of a file
 *
 * This is used in FileContentMatchers
 */
object FileLinesContent extends LinesContent[File] {
  def lines(f: File): Seq[String] =
    if (f.isDirectory) Seq()
    else FilePathReader.readLines(FilePath.unsafe(f)).runOption.getOrElse(Seq())

  def name(f: File) = f.getPath
}
