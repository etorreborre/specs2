package org.specs2.guide

import scala.io.Source
import org.specs2.control.Exceptions._

trait Specs2Variables {

  lazy val VERSION = versionLine.flatMap(extractVersion).getOrElse("version not found")

  lazy val SNAPSHOT_VERSION =
    if (IS_SNAPSHOT) VERSION
    else             NEXT_VERSION+"-SNAPSHOT"

  lazy val NEXT_VERSION =
    if (IS_SNAPSHOT) VERSION.replace("-SNAPSHOT", "")
    else {
      VERSION.split("\\.").toList match {
        case major :: minor :: patch :: _ => Seq(major, minor, patch.toInt+1).mkString(".")
        case major :: minor :: _          => Seq(major, minor, 1).mkString(".")
        case _                            => VERSION+".1"
      }
    }

  lazy val IS_SNAPSHOT = VERSION endsWith "SNAPSHOT"

  lazy val BRANCH = if (IS_SNAPSHOT) "master" else VERSION

  lazy val GUIDE_OFFICIAL_DIR = "guide/"
  lazy val GUIDE_SNAPSHOT_DIR = "guide-SNAPSHOT/"+GUIDE_OFFICIAL_DIR
  lazy val GUIDE_DIR          = (if (IS_SNAPSHOT) GUIDE_SNAPSHOT_DIR else GUIDE_OFFICIAL_DIR)

  lazy val IMAGES_OFFICIAL_DIR = "images/"
  lazy val IMAGES_SNAPSHOT_DIR = "../../"+IMAGES_OFFICIAL_DIR
  lazy val IMAGES_DIR          = (if (IS_SNAPSHOT) IMAGES_SNAPSHOT_DIR else IMAGES_OFFICIAL_DIR)

  lazy val EXAMPLES_OFFICIAL_DIR = "https://github.com/etorreborre/specs2/tree/"+VERSION+"/src/test/scala/examples"
  lazy val API_OFFICIAL_DIR      = "http://etorreborre.github.com/specs2/api/SPECS2-" + VERSION + "/"
  lazy val API_SNAPSHOT_DIR      = "http://etorreborre.github.com/specs2/api/master/"
  lazy val API_DIR               = (if (IS_SNAPSHOT) API_SNAPSHOT_DIR else API_OFFICIAL_DIR)

  private lazy val versionLine = buildSbt.flatMap(_.getLines.find(line => line contains "version"))
  private def extractVersion(line: String) = "\\s*version.*\\:\\=\\s*\"(.*)\"".r.findFirstMatchIn(line).map(_.group(1))
  private lazy val buildSbt = tryo(Source.fromFile("version.sbt"))((e:Exception) => println("can't find the version.sbt file "+e.getMessage))

}

object Specs2Variables extends Specs2Variables