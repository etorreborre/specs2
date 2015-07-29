package org.specs2
package analysis

import classycle._
import reflect._
import io._
import control._

/**
 * Implementation of the dependency finder using the classycle library.
 *
 * This implementation is faster and simpler than the CompilerDependencyFinder trait
 */
trait ClassycleDependencyFinder extends DependencyFinder {
  val fs = FilePathReader

  def getPackageDependents(sourceDir: DirectoryPath, targetDir: DirectoryPath): String => Action[Seq[Dependency]] = (packageName: String) => {
    fs.filePaths(targetDir, "**/*.class", verbose = false).map { paths =>
      val analyser = new Analyser(paths.map(_.path).toArray)
      analyser.createClassGraph()

      analyser.getClassGraph.collect {
        case classVertex if ClassName.packageName(classVertex.getAttributes.asInstanceOf[ClassAttributes].getName) == packageName =>
          (0 until classVertex.getNumberOfIncomingArcs).map { i =>
            Dependency(ClassName.className(classVertex.getAttributes.asInstanceOf[ClassAttributes].getName),
              ClassName.className(classVertex.getTailVertex(i).getAttributes.asInstanceOf[ClassAttributes].getName))
          }
      }.flatMap(identity).distinct.toSeq
    }
  }
}
