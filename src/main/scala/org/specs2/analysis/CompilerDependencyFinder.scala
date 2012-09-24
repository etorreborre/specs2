package org.specs2
package analysis

import org.specs2.io.fs
import scala.collection.mutable.{Map, HashMap}
import scala.tools.nsc._
import interactive._
import io._
import java.net.URLClassLoader
import java.net.URLDecoder
import reflect.PackageName._

/**
 * Implementation of the DependencyFinder trait using the compiler dependency analysis
 */
trait CompilerDependencyFinder extends DependencyFinder {

  /**
   * @return the class depending on the classes of a given package
   */
  def getPackageDependents(packageName: String, sourceDir: String, targetDir: String): Seq[Dependency] = {
    // load all dependencies for this source directory (compiles all files)
    val dependencies = sourceDependencies(sourceDir)
    // for each file in the package directory, get its dependencies
    packageDirectory(packageName, sourceDir).iterator.filter(_.path.endsWith(".scala")).toSeq flatMap { (file: AbstractFile) =>
      dependencies.dependentFiles(depth = 1, Set(file)).map { dependent =>
        Dependency(file, dependent, sourceDir)
      }
    }
  }

  /**
   * @return a seq of all scala files in the source directory
   */
  def selectFiles(sourceDir: String) = fs.filePaths(sourceDir, "**/*.scala")

  /** @return all the dependencies of source files in a given source directory by compiling them */
  private def sourceDependencies(sourceDir: String) =
      buildManager(sourceDir).compiler.dependencyAnalysis.dependencies

  private lazy val managers: Map[String, SimpleBuildManager] = new HashMap[String, SimpleBuildManager].withDefault { sourceDir =>
    val manager = new SimpleBuildManager(newSettings)
    manager.addSourceFiles(selectFiles(sourceDir).map(f => new PlainFile(f)).toSet)
    managers.put(sourceDir, manager)
    manager
  }
  /**
   * @return a new `BuildManager` for scala files in a source directory
   * the managers are memoized per sourceDir
   */
  private def buildManager(sourceDir: String) = managers(sourceDir)

  /**
   * @return a new Settings object for doing dependency analysis. It adds the current classpath and set-up a new directory for
   * adding the generated classes
   */
  private lazy val newSettings = {
    val settings = new Settings

    // the analysis output directory needs to be built before we run the analysis
    settings.classpath.value      = classPath.mkString(java.io.File.pathSeparator)
    settings.make.value           = "transitive"
    settings.stopAfter.value      = List("dependencyAnalysis")
    settings.skip.value           = List("flatten", "liftcode", "jvm")
    settings.dependencyfile.value = "xxx/the dependency file must not be saved"
    settings
  }

  /**
   * @return the current classPath as used by the current application
   */
  private lazy val classPath = {
    val cl = Thread.currentThread.getContextClassLoader
    if (cl.isInstanceOf[URLClassLoader]) {
      val ucl = cl.asInstanceOf[URLClassLoader]
      ucl.getURLs.foldLeft(Seq[String]()) { (res, url) =>
        if (url.getProtocol.equals("file")) res :+ new java.io.File(URLDecoder.decode(url.getPath, "UTF-8")).getCanonicalFile.getAbsolutePath
        else                                res
      }
    } else Seq[String]()
  }
  /**
   * @return a new package directory object from a source directory and a package name
   */
  private def packageDirectory(packageName: String, sourceDir: String) =
    new PlainDirectory(new Directory(new java.io.File(sourceDir+packageName.toPath)))
}
