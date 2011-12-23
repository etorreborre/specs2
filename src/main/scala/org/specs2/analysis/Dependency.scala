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

  /**
   * @return a String showing the dependency
   */
  def show: String =
    packageName(dependentClassName)+" -> "+packageName(className)+" ("+dependentClassName+" -> "+className+")"

  /**
   * @return a String showing that the actual dependency reveals a broken expected one
   */
  def showBreak: String =
    packageName(className)+" x-> "+packageName(dependentClassName)+" because "+dependentClassName+" -> "+className

  def dependsOn(names: Seq[String]) = names contains dependentPackageName
}

object Dependency {
  def apply(file: AbstractFile, dependent: AbstractFile, sourceDir: String) =
    new Dependency(scalaName(file, sourceDir), scalaName(dependent, sourceDir))

  private def scalaName(f: AbstractFile, sourceDir: String) = f.path.replace("\\", "/").replace(sourceDir, "").replace("/", ".").replace(".scala", "")
}

case class Dependencies(dependencies: Seq[Dependency]) {
  def isEmpty = dependencies.isEmpty
  def showBreaks = dependencies.map(_.showBreak).mkString("\n")
}

