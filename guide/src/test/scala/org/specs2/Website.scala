package org.specs2

import org.specs2.control._
import org.specs2.guide._
import io._
import org.specs2.specification.core.Env
import scalaz._, Scalaz._
import html._

class Website extends Specification with Specs2Variables { def is = br ^
  "create the website" ! createWebsite(outputDirectory)

  val outputDirectory = "target" / "specs2-reports"

  def createWebsite(outputDir: DirectoryPath) = { env: Env =>
    val fs = env.fileSystem
    val pages = List("index", "quickstart", "learn", "project", "nav").map(_ +".html").map(resource)
    val directories = List("css", "fonts", "images", "javascript").map(resourceDir)
    val vars = variables(env)

    pages.map { page =>
      for {
        _        <- directories.map(d => fs.copyDir(d, outputDirectory / d.name)).sequenceU
        template <- fs.readFile(page)
        replaced <- HtmlTemplate.runTemplate(template, vars)
        _        <- fs.writeFile(outputDir | page.name, replaced)
      } yield ()
    }.sequenceU.as(true)
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
