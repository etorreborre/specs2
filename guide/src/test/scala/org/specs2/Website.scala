package org.specs2

import control.*
import guide.*
import io.*
import specification.core.Env
import html.*
import runner.*
import main.*
import concurrent.ExecutionEnv
import org.specs2.fp.syntax.*
import java.net.{JarURLConnection, URI}
import java.nio.file.{Files, StandardCopyOption}
import scala.jdk.CollectionConverters.*

class Website(env: Env) extends Specification with Specs2Variables with Specs2Tags {
  def is = section("website") ^ sequential ^ s2"""

 create the website    $createWebsite
 create the user guide $createUserGuide
 copy unidoc           $copyUnidoc

"""

  given ExecutionEnv = env.executionEnv

  val outputDir = "target" / "specs2-reports" / "site"
  val versionDirName = FileName.unsafe(VERSION)

  def createWebsite = {
    val fs = env.fileSystem
    val pages = List("index", "quickstart", "learn", "project", "nav").map(_ + ".html").map(resource)
    val directories = List("css", "fonts", "images", "javascript").map(resourceDir)
    val vars = variables(env)
    val siteOutputDir = outputDir / "website" / versionDirName

    pages.map { page =>
      for
        _ <- directories.map(d => fs.copyDir(d, siteOutputDir / d.name)).sequence
        template <- fs.readFile(page)
        replacedVersion <- HtmlTemplate.runTemplate(template, vars)
        _ <-
          fs.writeFile(siteOutputDir | page.name, replacedVersion) >> {
            // copy the index page at the root of the site
            // it will then re-direct to a specific version
            if page.path.contains("index.html") then fs.writeFile(outputDir | page.name, replacedVersion)
            else Operation.unit
          }
      yield ()
    }.sequence >> writeVersionsFile(fs, siteOutputDir, vars("GUIDE_DIR"), vars("API_DIR")).map(_ => true)
  }

  def createUserGuide: Action[Boolean] = {
    val guideOutputDir = outputDir / "guide" / versionDirName
    val env1 = env.copy(arguments =
      Arguments.split(s"all html html.search html.toc html.nostats html.outdir ${guideOutputDir.dirPath}")
    )

    env1.fileSystem.copyFile(guideOutputDir / "css")(resource("css/specs2-user.css")).toAction >>
      ClassRunner.createClassRunner(env1).toAction.flatMap(_.run(UserGuide).map(_ => true))
  }

  def copyUnidoc: Action[Boolean] =
    val apiOutputDir = outputDir / "api" / versionDirName
    env.fileSystem.copyDir(DirectoryPath.unsafe("target/scala-"+BuildInfo.scalaVersion+"/unidoc"), apiOutputDir).toAction.map(_ => true)

  // HELPERS

  def writeVersionsFile(
      fs: FileSystem,
      siteOutputDir: DirectoryPath,
      guideDir: String,
      apiDir: String
  ): Operation[Unit] =
    publishedTags >>= (tags =>
      fs.writeFile(siteOutputDir / "javascript" | "versions.js", versionsJavaScript(tags, guideDir, apiDir))
    )

  def versionsJavaScript(tags: List[VersionTag], guideDir: String, apiDir: String): String = {
    def makeVersionVar(name: String, file: String) =
      s"""|var ${name}Versions
          | = [
          | ${tags
        .map(_.render)
        .map { tag =>
          val version = tag.replace("SPECS2-", "")
          s"""{id:"../../$name/$tag/$file", text:"$version"}"""
        }
        .mkString(",\n")}
          |];""".stripMargin

    makeVersionVar("guide", "org.specs2.guide.UserGuide.html") + "\n " +
    makeVersionVar("api", "index.html")
  }

  def variables(env: Env): Map[String, String] =
    specs2Variables.map { case (key, value) =>
      (key, env.arguments.commandLine.valueOr(key, value))
    }

  def resource(name: String): FilePath =
    FilePath.unsafe(resourceUri(name))

  def resourceDir(name: String): DirectoryPath =
    DirectoryPath.unsafe(resourceUri(name))

  private def resourceUri(name: String): URI =
    val url = Option(getClass.getClassLoader.getResource(name))
      .getOrElse(throw new IllegalArgumentException(s"Missing website resource: $name"))
    if url.getProtocol != "jar" then url.toURI
    else
      val connection = url.openConnection().asInstanceOf[JarURLConnection]
      connection.setUseCaches(false)
      val jar = connection.getJarFile
      val destination = (outputDir / "resources").toFile.toPath.toAbsolutePath.normalize()
      try
        jar.entries().asScala
          .filter(entry => entry.getName == name || entry.getName.startsWith(name + "/"))
          .foreach { entry =>
            val path = destination.resolve(entry.getName).normalize()
            require(path.startsWith(destination), s"Invalid website resource: ${entry.getName}")
            if entry.isDirectory then Files.createDirectories(path)
            else
              Files.createDirectories(path.getParent)
              val input = jar.getInputStream(entry)
              try Files.copy(input, path, StandardCopyOption.REPLACE_EXISTING)
              finally input.close()
          }
        destination.resolve(name).toUri
      finally jar.close()
}
