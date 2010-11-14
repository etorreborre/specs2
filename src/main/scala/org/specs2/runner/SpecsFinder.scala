package org.specs2
package runner

import java.util.regex._
import scala.collection.mutable.Queue
import io._
import reflect.Classes

trait SpecificationsFinder extends FileSystem with Classes with ConsoleOutput {

   /**
    * @param path a path to a directory containing scala files (it can be a glob: i.e. "dir/**/*spec.scala")
    * @param pattern a regular expression which is supposed to match an object name extending a Specification
    * @return specification names by scanning files and trying to find specifications declarations
    */
   def specificationNames(path: String, pattern: String) : List[String] = {
     filePaths(path) filter (_.endsWith(".scala")) flatMap { p =>
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
  def classNames(packageName: String, content: String, pattern: String, specType: String, suffix: String): List[String] = {
    def result(m: Matcher): Stream[String] = 
      if (m.find) { 
    	  val fullName = List(packageName, m.group(1).trim).mkString(".") + suffix   
    	  Stream.cons(fullName, result(m))
      }
      else Stream.empty
      
    val specPattern = "\\s*"+specType+"\\s*(" + pattern + ")\\s*extends\\s*.*"
    result(Pattern.compile(specPattern).matcher(content)).toList
  }
  /** @return the package declaration at the beginning of a file */
  def packageName(content: String): String = {
    def result(m: Matcher): Stream[String] = 
      if (m.find) Stream.cons(m.group(1).replace(";", "").trim, result(m))
      else Stream.empty
      
	  val pattern = "\\s*package\\s*(.+)\\s*"
    result(Pattern.compile(pattern).matcher(content)).mkString(".")
  }
  /**
   * @return a <code>Specification</code> object from a className if that class is a <code>Specification</code> class.<br>
   * Tries to load the class name and cast it to a specification
   *         None in case of an exception.
   */
  def createSpecification(className: String): Option[Specification] = tryToCreateObject[Specification](className)
  /**
   * @return a <code>Specification</code> object from a className if that class is a <code>Specification</code> class.<br>
   * Tries to load the class name and cast it to a specification
   *         None in case of an exception.
   */
  def createSpecification(className: String, printMessage: Boolean, printStackTrace: Boolean): Option[Specification] = createObject[Specification](className, printMessage, printStackTrace)
}
