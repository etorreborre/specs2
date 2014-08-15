package org.specs2
package io

import java.io.File
import text._

/**
 * Default implementation for reading lines out of a file
 */
object FileLinesContent extends LinesContent[File] with FileReader {
  def lines(f: File): Seq[String] = if (f.isDirectory) Seq() else readLines(FilePath.unsafe(f))
  def name(f: File) = f.getPath
}