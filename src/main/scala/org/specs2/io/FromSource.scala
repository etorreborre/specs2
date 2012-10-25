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
  
  private[specs2] lazy val srcTestDir: String = SystemProperties.getOrElse("srcTestDir", "src/test/scala").dirPath

  /**
   * get some source code by:
   *   - fetching the current stacktrace
   *   - finding the location of the example (6th trace by default)
   */
  def getCode(depth: Int = 6): Either[String, String] = getCodeFromTo(depth, depth)

  /**
   * get some source code by:
   *   - fetching the current stacktrace
   *   - finding the location of the example by taking the trace of the first line and the trace of the last line
   *    (at depth 6 and 9 by default)
   *
   * @return Left(error) if the startTrace and endTrace do not cover the same file
   *         Right(source code) if we find some code in the source file
   *
   */
  def getCodeFromTo(start: Int = 6, end: Int = 9, startLineOffset: Int = -1, endLineOffset: Int = -1): Either[String, String] = {
    val stackTrace = new Exception().getStackTrace()
    val (startTrace, endTrace) = (TraceLocation(stackTrace.apply(start)), TraceLocation(stackTrace.apply(end)))

    if (startTrace.fileName != endTrace.fileName)
      Left("No source file found at "+srcTestDir+startTrace.path)
    else {
      val (startLine, endLine) = (startTrace.lineNumber+startLineOffset, endTrace.lineNumber+endLineOffset)
      getCodeFromToWithLocation(startLine, endLine, startTrace)
    }
  }

  /**
   * get the source code of an example declared with the eg method as a prefix method
   */
  def getExampleFrom(depth: Int, lineOffset: Int): Either[String, String] = {
    val stackTrace = new Exception().getStackTrace()
    getCodeFromMethodCall(TraceLocation(stackTrace.apply(depth)))
  }

  /**
   * get the source code of an example declared with the eg method as a postfix method
   */
  def getExampleTo(depth: Int, lineOffset: Int): Either[String, String] = {
    val stackTrace = new Exception().getStackTrace()
    getCodeToMethodCall(TraceLocation(stackTrace.apply(depth)))
  }

  /**
   * @return the location of the current stacktrace, possibly filtered with a function
   */
  def location(stackFilter: Seq[StackTraceElement] => Seq[StackTraceElement]): TraceLocation = {
    val stackTrace = new Exception().getStackTrace().toSeq
    val filtered = stackFilter(stackTrace)
    TraceLocation(filtered.headOption.getOrElse(stackTrace(0)))
  }

  /**
   * @return lines of code specified by a start line and an end line in a file given by a TraceLocation
   */
  private def getCodeFromToWithLocation(startLine: Int, endLine: Int = 9, location: TraceLocation): Either[String, String] = {
    val path = srcTestDir+location.path

    if (endLine < startLine) {
      Left[String, String]("No source file found at "+path)
    } else {
      tryOr {
        val content = readLines(path)
        if (startLine >= 0) {
          val code = ((startLine to endLine) map content).mkString("\n")
          Right[String, String](code): Either[String, String]
        } else Left[String, String]("Unvalid start line: "+startLine+" for file: "+path)

      } { e => Left[String, String]("No source file found at "+path) }
    }
  }

  /**
   * @return lines of code after a method call, as specified by a TraceLocation.
   * The end of the method call is given by the first '}' character that is encountered
   */
  private def getCodeFromMethodCall(location: TraceLocation): Either[String, String] = {
    val (path, line) = (srcTestDir+location.path, location.lineNumber - 1)

    tryOr {
      val content = readLines(path)
      if (line >= 0) {
        val (start, last) = content.drop(line - 1).span(l => !l.contains("}"))
        Right[String, String]((start ++ last.take(1)).mkString("\n")): Either[String, String]
      } else Left[String, String]("Unvalid start line: "+line+" for file: "+path)

    } { e => Left[String, String]("No source file found at "+path) }
  }

  /**
   * @return lines of code before a method call (a postfix method), as specified by a TraceLocation.
   * The beginning of the call is given by the first '{' character that is encountered
   */
  private def getCodeToMethodCall(location: TraceLocation): Either[String, String] = {
    val (path, line) = (srcTestDir+location.path, location.lineNumber - 1)

    tryOr {
      val content = readLines(path)
      if (line >= 0) {
        val toMethodCall = content.dropRight(content.size - (line + 1))
        val (start, last) = toMethodCall.reverse.span(l => !l.contains("{"))
        val code = last.take(1) ++ start.reverse
        Right[String, String](code.mkString("\n")): Either[String, String]
      } else Left[String, String]("Unvalid start line: "+line+" for file: "+path)

    } { e => Left[String, String]("No source file found at "+path) }
  }
}

private[specs2]
object FromSource extends FromSource