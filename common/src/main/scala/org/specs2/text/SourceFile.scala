package org.specs2
package text

import java.util.regex.{Matcher, Pattern}
import control._
import scalaz.syntax.bind._

/**
 * Utility methods to parse the contents of source files
 */
private[specs2]
trait SourceFile {
  private lazy val CLASSNAME_REGEX = "([\\p{L}_$][\\p{L}\\p{N}_$]*\\.)*[\\p{L}_$][\\p{L}\\p{N}_$]*".r

  /**
   * extract the class names corresponding to a pattern found in a content
   *
   * @param packageName the base package for the class names
   * @param content content of a source file
   * @param pattern a regular expression for a class name
   */
  def classNames(packageName: String, content: String, pattern: Pattern, suffix: String, verbose: Boolean): Action[Seq[String]] = {
    def result(m: Matcher): Stream[String] =
      if (m.find) {
        val fullName = List(packageName, m.group(1).trim).mkString(".") + suffix
        Stream.cons(fullName, result(m))
      } else Stream.empty

    val found = result(pattern.matcher(content)).toList
    log("  found classes: "+found.mkString(", "), verbose && found.nonEmpty) >>
      Actions.safe(found.filter(c => CLASSNAME_REGEX.pattern.matcher(c).matches))
  }

  /** @return the package name corresponding to the package declarations at the beginning of a file */
  def packageName(content: String): String = {
    def result(m: Matcher): Stream[String] =
      if (m.find) Stream.cons(m.group(1).replace(";", "").trim, result(m))
      else Stream.empty

    val pattern = "\\s*package\\s*(.+)\\s*"

    // extract the packages section at the beginning of the file
    val packages = content.split("\n").filter(_.trim.startsWith("package")).mkString("\n")
    result(Pattern.compile(pattern).matcher(packages)).mkString(".")
  }

}

private[specs2]
object SourceFile extends SourceFile

