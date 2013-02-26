package org.specs2
import control.Exceptions._
import scala.io.Source

object Specs2Variables {

  lazy val version         = versionLine.flatMap(extractVersion).getOrElse("version not found")

  lazy val snapshotVersion =
    if (isSnapshot) version
    else            nextVersion+"-SNAPSHOT"

  lazy val nextVersion =
    if (isSnapshot) version.replace("-SNAPSHOT", "")
    else {
      version.split("\\.").toList match {
        case major :: minor :: patch :: _ => Seq(major, minor, patch.toInt+1).mkString(".")
        case major :: minor :: _          => Seq(major, minor, 1).mkString(".")
        case _                            => version+".1"
      }
    }

  lazy val isSnapshot = version endsWith "SNAPSHOT"

  lazy val branch = if (isSnapshot) "master" else version

  lazy val guideOfficialDir = "guide/"
  lazy val guideSnapshotDir = "guide-SNAPSHOT/"+guideOfficialDir
  lazy val guideDir         = (if (isSnapshot) guideSnapshotDir else guideOfficialDir)

  lazy val examplesOfficialDir = "https://github.com/etorreborre/specs2/tree/"+version+"/src/test/scala/examples"
  lazy val apiOfficialDir      = "http://etorreborre.github.com/specs2/api/SPECS2-" + version + "/"
  lazy val apiSnapshotDir      = "http://etorreborre.github.com/specs2/api/master/"
  lazy val apiDir              = (if (isSnapshot) apiSnapshotDir else apiOfficialDir)

  private lazy val versionLine = buildSbt.flatMap(_.getLines.find(line => line contains "version"))
  private def extractVersion(line: String) = "\\s*version.*\\:\\=\\s*\"(.*)\"".r.findFirstMatchIn(line).map(_.group(1))
  private lazy val buildSbt = tryo(Source.fromFile("version.sbt"))((e:Exception) => println("can't find the version.sbt file "+e.getMessage))

  implicit def toVersionedText(t: String): VersionedText = VersionedText(t)
  case class VersionedText(t: String) {
    /**
     * set the version and branch tags in the pages
     */
    def replaceVariables = {
      Seq("VERSION"            -> version,
          "SNAPSHOT_VERSION"   -> snapshotVersion,
          "NEXT_VERSION"       -> nextVersion,
          "BRANCH"             -> branch,
          "API"                -> apiDir,
          "API_OFFICIAL"       -> apiOfficialDir,
          "API_SNAPSHOT"       -> apiSnapshotDir,
          "EXAMPLES_OFFICIAL"  -> examplesOfficialDir,
          "GUIDE"              -> guideDir,
          "GUIDE_OFFICIAL"     -> guideOfficialDir,
          "GUIDE_SNAPSHOT"     -> guideSnapshotDir).foldLeft(t) { case (res, (k, v)) => res.replaceAll("\\$\\{SPECS2_"+k+"\\}", v) }
    }
  }

}