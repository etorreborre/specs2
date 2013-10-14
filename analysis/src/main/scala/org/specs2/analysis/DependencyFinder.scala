package org.specs2
package analysis

/**
 * This trait provides a way to analyse the dependencies of a given package
 */
trait DependencyFinder {
  /**
   * @return the class depending on the classes of a given package
   */
  def getPackageDependents(packageName: String, sourceDir: String, targetDir: String): Seq[Dependency]
}

