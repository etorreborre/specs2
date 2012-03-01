package org.specs2
package runner

import java.util.regex._
import io._
import reflect.Classes
import specification.SpecificationStructure

/**
 * This trait loads specifications found on a given source directory based
 * on a regular expression representing the Specification name, usually .*Spec
 */
trait SpecificationsFinder extends FileSystem with Classes with ConsoleOutput {

  /**
   * @param path a path to a directory containing scala files (it can be a glob: i.e. "dir/**/*spec.scala")
   * @param pattern a regular expression which is supposed to match an object name extending a Specification
   * @param filter a function to filter out unwanted specifications
   * @return specifications created from specification names
   */
  def specifications(path: String = "**/*.scala",
                     pattern: String = ".*Spec",
                     filter: String => Boolean = { (name: String) => true },
                     basePath: String = FromSource.srcDir,
                     verbose: Boolean = false): Seq[SpecificationStructure] =
    specificationNames(path, pattern, basePath, verbose).view.filter(filter).flatMap(n => createSpecification(n))
  /**
   * @param path a path to a directory containing scala files (it can be a glob: i.e. "dir/**/*spec.scala")
   * @param pattern a regular expression which is supposed to match an object name extending a Specification
   * @return specification names by scanning files and trying to find specifications declarations
   */
  def specificationNames(path: String = "**/*.scala",
                         pattern: String = ".*Spec",
                         basePath: String = FromSource.srcDir,
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
   * adds possible specification class names found in the file <code>filePath</code><br>
   * The specification pattern is: "\\s*object\\s*(" + pattern + ")\\s*extends\\s*.*Spec.*\\s*\\{"
   * This may be later extended to support other arbitrary patterns
   *
   * @param path a path to a directory containing scala files (it can be a glob: i.e. "dir/**/*spec.scala")
   * @param content content of the file
   * @param pattern a regular expression which is supposed to match an object name extending a Specification
   */
  def classNames(packageName: String, content: String, pattern: Pattern, suffix: String, verbose: Boolean = false): Seq[String] = {

    def result(m: Matcher): Stream[String] = 
      if (m.find) { 
    	  val fullName = List(packageName, m.group(1).trim).mkString(".") + suffix   
    	  Stream.cons(fullName, result(m))
      } else Stream.empty

    val found = result(pattern.matcher(content)).toList
    if (verbose && found.nonEmpty) println("found the following specifications: "+found.mkString(","))
    found
  }

  /**
   * pattern to use to get specification names from file contents
   */
  def specPattern(specType: String, pattern: String) = "\\s*"+specType+"\\s*(" + pattern + ")\\s*extends\\s*.*"

  /** @return the package declaration at the beginning of a file */
  def packageName(content: String): String = {
    def result(m: Matcher): Stream[String] = 
      if (m.find) Stream.cons(m.group(1).replace(";", "").trim, result(m))
      else Stream.empty
      
	  val pattern = "\\s*package\\s*(.+)\\s*"
    result(Pattern.compile(pattern).matcher(content)).mkString(".")
  }
  /**
   * @return a <code>SpecificationStructure</code> object from a className if that class is a <code>SpecificationStructure</code> class.<br>
   * Tries to load the class name and cast it to a specification
   *         None in case of an exception.
   */
  def createSpecification(className: String): Option[SpecificationStructure] =
    tryToCreateObject[SpecificationStructure](className)
  /**
   * @return a <code>SpecificationStructure</code> object from a className if that class is a <code>SpecificationStructure</code> class.<br>
   * Tries to load the class name and cast it to a specification
   *         None in case of an exception.
   */
  def createSpecification(className: String, printMessage: Boolean, printStackTrace: Boolean): Option[SpecificationStructure] =
    createObject[SpecificationStructure](className, printMessage, printStackTrace)
}

object SpecificationsFinder extends SpecificationsFinder