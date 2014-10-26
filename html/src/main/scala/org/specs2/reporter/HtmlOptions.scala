package org.specs2
package reporter

import java.io.File
import io._

/** Options for the Html generation */
case class HtmlOptions(outDir: DirectoryPath, baseDir: DirectoryPath, template: FilePath, variables: Map[String, String], noStats: Boolean, createIndex: Boolean) {
  def javascriptDir = outDir / "javascript"
  def indexDir      = javascriptDir / "tipuesearch"
  def indexFile     = indexDir | "tipuesearch_contents.js"
}

object HtmlOptions {
  val outDir    = DirectoryPath.unsafe(new File("target/specs2-reports").getAbsoluteFile)
  val baseDir   = DirectoryPath.unsafe(".")
  val variables = Map[String, String]()
  val noStats   = false
  val createIndex = false

  def template(outDir: DirectoryPath): FilePath =
    outDir / "templates" | "specs2.html"
}

