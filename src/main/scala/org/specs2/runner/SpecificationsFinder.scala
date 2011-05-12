package org.specs2
package runner

import java.util.regex._
import scala.collection.mutable.Queue
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
   * @return specifications created from specification names
   */
  def specifications(path: String = "*", pattern: String = ".*Spec", basePath: String = FromSource.srcDir): Seq[SpecificationStructure] =
    specificationNames(path, pattern, basePath).flatMap(n => createSpecification(n))
  /**
   * @param path a path to a directory containing scala files (it can be a glob: i.e. "dir/**/*spec.scala")
   * @param pattern a regular expression which is supposed to match an object name extending a Specification
   * @return specification names by scanning files and trying to find specifications declarations
   */
  def specificationNames(path: String = "*", pattern: String = ".*Spec", basePath: String = FromSource.srcDir) : Seq[String] = {
     filePaths(basePath, path) filter (_.endsWith(".scala")) flatMap { p =>
       val fileContent = readFile(p)
       val packName = packageName(fileContent)
       classNames(packName, fileContent, pattern, "object", "$") ++ classNames(packName, fileContent, pattern, "class", "")	 
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
  def classNames(packageName: String, content: String, pattern: String, specType: String, suffix: String): Seq[String] = {
    def result(m: Matcher): Stream[String] = 
      if (m.find) { 
    	  val fullName = List(packageName, m.group(1).trim).mkString(".") + suffix   
    	  Stream.cons(fullName, result(m))
      }
      else Stream.empty
      
    result(Pattern.compile(specPattern(specType, pattern)).matcher(content)).toList
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
