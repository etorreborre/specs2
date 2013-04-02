package org.specs2

import guide.Specs2Variables
import io.FileSystem
import scala.io.Source
import Specs2Variables._

class Index extends Specification with FileSystem { def is =
  "created a new index page"    ! createPage("index.html")^
  "created a new sponsors page" ! createPage("sponsors.html")

  def createPage(template: String, resourcesDir: String = "src/main/resources/", outputDir: String = "target/specs2-reports/") = {
    writeFile(outputDir+template,
      Source.fromFile(resourcesDir+template).getLines().map(_.replaceVariables).mkString("\n"))
    ok
  }
}
