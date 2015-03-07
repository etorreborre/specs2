package org.specs2
package guide

import info.guide.BuildInfo
import control._
import io._

object Specs2Variables extends Specs2Variables

trait Specs2Variables {

  lazy val VERSION        = BuildInfo.version
  lazy val GUIDE_DIR      = s"http://etorreborre.github.io/specs2/guide/SPECS2-$VERSION"
  lazy val SITE_DIR       = s"http://etorreborre.github.io/specs2/website/SPECS2-$VERSION"
  lazy val API_DIR        = s"http://etorreborre.github.io/specs2/api/SPECS2-$VERSION"
  lazy val PROJECT_DIR    = s"https://github.com/etorreborre/specs2/tree/SPECS2-$VERSION"
  lazy val EXAMPLES_DIR   = s"$PROJECT_DIR/examples/src/test/scala/examples"

  lazy val specs2Variables: Map[String, String] =
    Map(
      "VERSION"           -> VERSION,
      "SITE_DIR"          -> SITE_DIR,
      "PROJECT_DIR"       -> PROJECT_DIR,
      "GUIDE_DIR"         -> GUIDE_DIR,
      "EXAMPLES_DIR"      -> EXAMPLES_DIR,
      "API_DIR"           -> API_DIR
    )


  /** @return true if the version is not timestamped */
  def isOfficial(version: String): Boolean =
    !version.contains(BuildInfo.date)
}

trait Specs2Tags {
  def allTags: Action[List[String]] =
    Executable.execute(FilePath("git"), Seq("tag")).map(_.trim.split("\n").map(_.trim).toList)

  def publishedTags: Action[List[String]] =
    allTags.map(_.filter { tag =>
      (tag == "SPECS2-2.4.17" ||
       tag.startsWith("SPECS2-3.")) &&
      !tag.contains("M")
    })
}