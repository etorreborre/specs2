package org.specs2
package analysis

import io.DirectoryPath
import control.Action

/**
 * This trait provides a way to analyse the dependencies of a given package
 */
trait DependencyFinder {
  /**
   * @return the classes depending on the classes of package, given its name
   */
  def getPackageDependents(sourceDir: DirectoryPath, targetDir: DirectoryPath): String => Action[Seq[Dependency]]
}

