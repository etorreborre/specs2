package org.specs2
package analysis

import tools.nsc.io.AbstractFile

/**
 * This class represents a dependency between two classes in the user's project
 */
case class Dependency(className: String, dependentClassName: String) {
  private def packageName(n: String) = n.split("\\.").headOption.getOrElse("")
  def dependentPackageName = dependentClassName.split("\\.").headOption.getOrElse("")
  def show(showBreak: Boolean = true) = {
    packageName(className)+" -> "+packageName(dependentClassName)+(if (showBreak) " ("+className+" -> "+dependentClassName+")" else "")
  }

  def dependsOn(names: Seq[String]) = names contains dependentPackageName
}

object Dependency {
  def apply(file: AbstractFile, dependent: AbstractFile, sourceDir: String) =
    new Dependency(scalaName(file, sourceDir), scalaName(dependent, sourceDir))

  private def scalaName(f: AbstractFile, sourceDir: String) = f.path.replace("\\", "/").replace(sourceDir, "").replace("/", ".").replace(".scala", "")
}

case class Dependencies(dependencies: Seq[Dependency]) {
  def isEmpty = dependencies.isEmpty
  def show(showAllBreaks: Boolean = true) = dependencies.map(_.show(showAllBreaks)).mkString("\n")
}

