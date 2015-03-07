package org.specs2

import control._
import guide._
import io._
import specification.core.Env
import scalaz._, Scalaz._
import html._
import runner._
import main._

class Website extends Specification with Specs2Variables with Specs2Tags { def is = s2"""

 create the website    $createWebsite
 create the user guide $createUserGuide

"""

  val outputDir = "target" / "specs2-reports" / "site"
  val versionDirName = FileName.unsafe("SPECS2-"+VERSION)

  def createWebsite = { env: Env =>
    val fs = env.fileSystem
    val pages = List("index", "quickstart", "learn", "project", "nav").map(_ +".html").map(resource)
    val directories = List("css", "fonts", "images", "javascript").map(resourceDir)
    val vars = variables(env)
    val siteOutputDir = outputDir / "website" / versionDirName

    pages.map { page =>
      for {
        _               <- directories.map(d => fs.copyDir(d, siteOutputDir / d.name)).sequenceU
        template        <- fs.readFile(page)
        replacedVersion <- HtmlTemplate.runTemplate(template, vars)
        _               <-
          fs.writeFile(siteOutputDir | page.name, replacedVersion) >> {
            // copy the index page at the root of the site
            // it will then re-direct to a specific version
            if (page.path.contains("index.html") && isOfficial(VERSION)) fs.writeFile(outputDir | page.name, replacedVersion)
            else Actions.ok(())
          }
      } yield ()
    }.sequenceU >> writeVersionsFile(fs, siteOutputDir, vars("GUIDE_DIR"), vars("API_DIR")).as(true)
  }

  def createUserGuide = { env1: Env =>
    val guideOutputDir = outputDir / "guide" / versionDirName
    val env = env1.copy(arguments = Arguments.split(s"all html console html.search html.toc html.nostats html.outdir ${guideOutputDir.dirPath}"))
    ClassRunner.report(env)(UserGuide).as(true)
  }

  def writeVersionsFile(fs: FileSystem, siteOutputDir: DirectoryPath, guideDir: String, apiDir: String): Action[Unit] =
    publishedTags >>= (tags => fs.writeFile(siteOutputDir / "javascript" | "versions.js", versionsJavaScript(tags, guideDir, apiDir)))

  def versionsJavaScript(tags: List[String], guideDir: String, apiDir: String): String = {
    def makeVersionVar(name: String) =
      s"""|var ${name}Versions = [
          | ${tags.map(tag => s"""{id:"$name/$tag", text:"${tag.replace("SPECS2-", "")}"}""").mkString(",\n")}
          |];""".stripMargin

    makeVersionVar("guide")+"\n"+
    makeVersionVar("api")
  }


  def variables(env: Env): Map[String, String] =
    specs2Variables.map { case (key, value) =>
      (key, env.arguments.commandLine.valueOr(key, value))
    }

  def resource(name: String): FilePath =
    FilePath.unsafe(getClass.getClassLoader.getResource(name).toURI)


  def resourceDir(name: String): DirectoryPath =
    DirectoryPath.unsafe(getClass.getClassLoader.getResource(name).toURI)
}
