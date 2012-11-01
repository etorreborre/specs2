package org.specs2
package runner

import java.util.regex._
import io._
import reflect.Classes
import specification.SpecificationStructure
import main.Arguments
import text.SourceFile

/**
 * This trait loads specifications found on a given source directory based
 * on a regular expression representing the Specification name, usually .*Spec
 */
trait SpecificationsFinder extends FileSystem with Classes with ConsoleOutput with SourceFile {
  /**
   * @param path a path to a directory containing scala files (it can be a glob: i.e. "dir/**/*spec.scala")
   * @param pattern a regular expression which is supposed to match an object name extending a Specification
   * @param filter a function to filter out unwanted specifications
   * @return specifications created from specification names
   */
  def specifications(path: String = "**/*.scala",
                     pattern: String = ".*Spec",
                     filter: String => Boolean = { (name: String) => true },
                     basePath: String = FromSource.srcTestDir,
                     verbose: Boolean = false)
                    (implicit args: Arguments = Arguments()): Seq[SpecificationStructure] = {
    specificationNames(path, pattern, basePath, verbose).view.filter(filter).
      flatMap(n => createSpecification(n, verbose))
  }
  /**
   * @param path a path to a directory containing scala files (it can be a glob: i.e. "dir/**/*spec.scala")
   * @param pattern a regular expression which is supposed to match an object name extending a Specification
   * @return specification names by scanning files and trying to find specifications declarations
   */
  def specificationNames(path: String = "**/*.scala",
                         pattern: String = ".*Spec",
                         basePath: String = FromSource.srcTestDir,
                         verbose: Boolean = false) : Seq[String] = {
    lazy val specClassPattern = {
      val p = specPattern("class", pattern)
      if (verbose) println("\nthe pattern used to match specification classes is: "+p+"\n")
      Pattern.compile(p)
    }
    lazy val specObjectPattern = {
      val p = specPattern("object", pattern)
      if (verbose) println("\nthe pattern used to match specification objects is: "+p+"\n")
      Pattern.compile(p)
    }
    filePaths(basePath, path, verbose) filter (_.endsWith(".scala")) flatMap { p =>
      val fileContent = readFile(p)
      val packName = packageName(fileContent)
      val (objectPattern, classPattern) = (specObjectPattern, specClassPattern)
      if (verbose) println("\nSearching for specifications in file: "+p)
      classNames(packName, fileContent, objectPattern, "$", verbose) ++ classNames(packName, fileContent, classPattern, "", verbose)
    }
   }

  /**
   * pattern to use to get specification names from file contents
   */
  def specPattern(specType: String, pattern: String) = "\\s*"+specType+"\\s*(" + pattern + ")\\s*extends\\s*.*"

  /**
   * @return a <code>SpecificationStructure</code> object from a className if that class is a <code>SpecificationStructure</code> class.<br>
   * Tries to load the class name and cast it to a specification
   *         None in case of an exception.
   */
  def createSpecification(className: String, verbose: Boolean = false)(implicit args: Arguments): Option[SpecificationStructure] = {
    SpecificationStructure.createSpecificationEither(className) match {
      case Right(s) => {
        if (verbose) { println("created specification instance for "+className) }
        Some(s)
      }
      case Left(e)  => {
        if (verbose) { println(e.getMessage); e.printStackTrace }
        None
      }
    }
  }

  /**
   * @return a <code>SpecificationStructure</code> object from a className if that class is a <code>SpecificationStructure</code> class.<br>
   * Tries to load the class name and cast it to a specification
   *         None in case of an exception.
   */
  def createSpecification(className: String, printMessage: Boolean, printStackTrace: Boolean): Option[SpecificationStructure] =
    createObject[SpecificationStructure](className, printMessage, printStackTrace)
}

object SpecificationsFinder extends SpecificationsFinder