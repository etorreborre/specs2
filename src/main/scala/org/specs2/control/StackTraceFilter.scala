package org.specs2
package control
import Throwablex._
import data.IncludedExcluded
import text.Trim._

/**
 * This trait filters an Exception stacktrace
 */
trait StackTraceFilter {
  /** @return the filtered stacktrace */
  def apply(e: Seq[StackTraceElement]): Seq[StackTraceElement]
  /** @return an exception with a filtered stacktrace */
  def apply(e: Exception): Exception = exception(e.getMessage, apply(e.getFullStackTrace), e.getCause)
}

/**
 * Implementation of the StackTraceFilter trait with a list of include/exclude patterns
 */
case class IncludeExcludeStackTraceFilter(include: Seq[String], exclude: Seq[String]) extends StackTraceFilter { outer =>
  private val filter = new IncludedExcluded[StackTraceElement] {
    val matchFunction = (st: StackTraceElement, patterns: Seq[String]) => patterns.exists(p => st.toString matches (".*"+p+".*"))
    val include = outer.include
    val exclude = outer.exclude
  }
  /** add include patterns */
  def includeAlso(patterns: String*) = copy(include = this.include ++ patterns)
  /** add exclude patterns */
  def excludeAlso(patterns: String*) = copy(exclude = this.exclude ++ patterns)

  /** filter an Exception stacktrace */
  def apply(st: Seq[StackTraceElement]) = st.filter(filter.keep)
}

/**
 * Factory object to build a stack trace filter from include/exclude expressions:
 *
 * .*specs2                       ==> include .*specs2 traces
 * .*specs2/scala.*               ==> include .*specs2 traces, exclude scala.* traces
 * .*specs2,scala/scalaz,eclipse  ==> include .*specs2,scala traces, exclude scalaz and eclipse traces
 *
 */
object IncludeExcludeStackTraceFilter {
  def fromString(s: String): StackTraceFilter = {
    val splitted = s.split("/").toSeq
    if (splitted.size == 0)
      new IncludeExcludeStackTraceFilter(Seq[String](), Seq[String]())
    else if (splitted.size == 1)
      new IncludeExcludeStackTraceFilter(splitted(0).splitTrim(","), Seq[String]())
    else if (splitted.size == 2)
      new IncludeExcludeStackTraceFilter(splitted(0).splitTrim(","), splitted(1).splitTrim(","))
    else
      new IncludeExcludeStackTraceFilter(splitted(0).splitTrim(","), splitted.drop(1).mkString(",").splitTrim(","))
  }
}
/**
 * default filter for specs2 runs
 */
object DefaultStackTraceFilter extends
  IncludeExcludeStackTraceFilter(Seq(),
    Seq("org.specs2", "scalaz\\.",
        "java\\.", "scala\\.",
        "sbt\\.", "com.intellij", "org.junit", "org.eclipse.jdt"))

/**
 * This filter doesn't do anything
 */
object NoStackTraceFilter extends StackTraceFilter {
  /** @return the filtered stacktrace */
  def apply(e: Seq[StackTraceElement]) = e
}
