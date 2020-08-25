package org.specs2
package text

import java.util.regex.{Matcher, Pattern}
import control._
import fp.syntax._

/**
 * Utility methods to parse the contents of source files
 */
private[specs2]
case class SourceFile(logger: Logger):
  private lazy val CLASSNAME_REGEX = "([\\p{L}_$][\\p{L}\\p{N}_$]*\\.)*[\\p{L}_$][\\p{L}\\p{N}_$]*".r

  /**
   * extract the class names corresponding to a pattern found in a content
   *
   * @param packageName the base package for the class names
   * @param content content of a source file
   * @param pattern a regular expression for a class name
   */
  def classNames(packageName: String, content: String, pattern: Pattern, suffix: String, verbose: Boolean): Operation[List[String]] =
    def result(m: Matcher): LazyList[String] =
      if (m.find && m.groupCount >= 1)
        val fullName =
          if (packageName.isEmpty) m.group(1).trim + suffix
          else                     List(packageName, m.group(1).trim).mkString(".") + suffix
        LazyList.cons(fullName, result(m))
      else LazyList.empty

    val found = result(pattern.matcher(content)).toList
    logger.info("  found classes: "+found.mkString(", "), verbose && found.nonEmpty) >>
      Operation.delayed(found.filter(c => CLASSNAME_REGEX.pattern.matcher(c).matches))

  /** @return the package name corresponding to the package declarations at the beginning of a file */
  def packageName(content: String): String =
    def result(m: Matcher): LazyList[String] =
      if (m.find) LazyList.cons(m.group(1).replace(";", "").trim, result(m))
      else LazyList.empty

    val pattern = "\\s*package\\s*?([^\\s/]+).*"

    // extract the packages section at the beginning of the file
    val packages = content.split("\n").filter(_.trim.startsWith("package")).mkString("\n")
    result(Pattern.compile(pattern).matcher(packages)).mkString(".")

