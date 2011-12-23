package org.specs2
package analysis

import scala.tools.nsc.io.AbstractFile
import reflect.ClassName

/**
 * This class represents a dependency between two classes in the user's project
 */
case class Dependency(className: String, dependentClassName: String) {
  private def packageName(n: String) = ClassName.packageName(n)
  def dependentPackageName = packageName(dependentClassName)

  def show: String = show(true)
  def show(showBreak: Boolean): String = {
    packageName(dependentClassName)+" -> "+packageName(className)+(if (showBreak) " ("+dependentClassName+" -> "+className+")" else "")
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

