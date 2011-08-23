package org.specs2
package io

import io.FileReader._
import io.Paths._
import control.Throwablex._
import control.Exceptions._
import main.SystemProperties
import control.TraceLocation

/**
 * Get source code from the current point of execution
 * 
 * There are constants in this trait designed to grab exactly the proper stacktraces, corresponding to the code in the specification
 *
 * It must be noted that this only work if the code being executed is in a file which has the same name as the class containing the code.
 * 
 * The source dir is assumed to be "src/test/scala/" by default but this can be modified by setting the "specs2.srcTestDir" System property
 *
 */
private[specs2]
trait FromSource {
  
  private[specs2] lazy val srcDir: String = SystemProperties.getOrElse("srcTestDir", "src/test/scala").dirPath

  /**
   * get some source code by:
   *   * fetching the current stacktrace
   *   * finding the location of the example (6th trace by default)
   */
  def getCode(depth: Int = 6): Either[String, String] = getCodeFromTo(depth, depth)

  /**
   * get some source code by:
   *   * fetching the current stacktrace
   *   * finding the location of the example by taking the trace of the first line and the trace of the last line
   *    (at depth 6 and 9 by default)
   *
   * @return Left(error) if the startTrace and endTrace do not cover the same file
   *         Right(source code) if we find some code in the source file
   *
   */
  def getCodeFromTo(start: Int = 6, end: Int = 9, startLineOffset: Int = -1, endLineOffset: Int = -1): Either[String, String] = {
    val stackTrace = new Exception().getStackTrace()
    val (startTrace, endTrace) = (new TraceLocation(stackTrace.apply(start)), new TraceLocation(stackTrace.apply(end)))

    if (startTrace.fileName != endTrace.fileName)
      Left("No source file found at "+srcDir+startTrace.path)
    else {
      val (startLine, endLine) = (startTrace.lineNumber+startLineOffset, endTrace.lineNumber+endLineOffset)
      val stackFilter = (st: Seq[StackTraceElement]) => st.filter(_.toString.contains(".getSourceCode(")).drop(1)
      getCodeFromToWithLocation(startLine, endLine, location(stackFilter))
    }
  }

  /**
   * @return the location of the current stacktrace, possibly filtered with a function
   */
  def location(stackFilter: Seq[StackTraceElement] => Seq[StackTraceElement]): TraceLocation = {
    val stackTrace = new Exception().getStackTrace().toList
    val trace = stackFilter(stackTrace).headOption
    new TraceLocation(trace.getOrElse(stackTrace(0)))
  }

  private def getCodeFromToWithLocation(startLine: Int, endLine: Int = 9, location: TraceLocation): Either[String, String] = {
    if (endLine < startLine) {
      Left[String, String]("No source file found at "+srcDir+location.path)
    } else {
      tryOr {
        val content = readLines(srcDir+location.path)
        val code = ((startLine to endLine) map content).mkString("\n")
        Right[String, String](code): Either[String, String]
      } { e => Left[String, String]("No source file found at "+srcDir+location.path) }
    }
  }

}

private[specs2]
object FromSource extends FromSource