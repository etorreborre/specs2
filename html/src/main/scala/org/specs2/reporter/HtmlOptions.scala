package org.specs2
package reporter

import java.io.File
import io._

/** Options for the Html generation */
case class HtmlOptions(
  outDir:             DirectoryPath,
  baseDir:            DirectoryPath,
  template:           FilePath,
  variables:          Map[String, String],
  noStats:            Boolean,
  search:             Boolean,
  warnMissingSeeRefs: Boolean,
  tocEntryMaxSize:    Int,
  toc:                Boolean) {

  def javascriptDir = outDir / "javascript"
  def indexDir      = javascriptDir / "tipuesearch"
  def indexFile     = indexDir | "tipuesearch_contents.js"
  def sidebar       = toc || search

  def templateVariables =
    variables
      .updated("baseDir",        baseDir.path)
      .updated("outDir",         outDir.path)
      .updated("template",       template.path)
      .updateWhenTrue("sidebar", sidebar)
      .updateWhenTrue("nostats", noStats)
      .updateWhenTrue("search",  search)
      .updateWhenTrue("toc",     toc)

  implicit class Update(map: Map[String, String]) {
    def updateWhenTrue(name: String, value: String, condition: Boolean): Map[String, String] =
      if (condition) map.updated(name, value)
      else map

    def updateWhenTrue(name: String, value: Boolean): Map[String, String] =
      map.updateWhenTrue(name, value.toString, value)
  }
}

object HtmlOptions {

  val outDir             = DirectoryPath.unsafe(new File("target/specs2-reports").getAbsoluteFile)
  val baseDir            = DirectoryPath.unsafe(".")
  val variables          = Map[String, String]()
  val noStats            = false
  val search             = false
  val toc                = false
  val tocEntryMaxSize    = 18
  val warnMissingSeeRefs = true


  def template(outDir: DirectoryPath): FilePath =
    outDir / "templates" | "specs2.html"
}

