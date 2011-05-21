package org.specs2
package io

import io.FileReader._
import io.Paths._
import control.Throwablex._
import control.Exceptions._
import main.SystemProperties

/**
 * Get source code from the current point of execution
 * 
 * * An important limitation is that only the content of one line will be returned *
 *
 * It must also be noted that this only work if the code being executed is in a file which has the same name as
 * the class containing the code.
 * 
 * The source dir is assumed to be "src/test/scala/" by default but this can be modified
 * by setting the "specs2.srcTestDir" System property
 *
 */
trait FromSource {
  
  private[specs2] lazy val srcDir: String = SystemProperties.getOrElse("srcTestDir", "src/test/scala").dirPath

  /**
   * get some source code by:
   *   * fetching the current stacktrace
   *   * finding the location of the example (4th trace by default)
   */
  def getCode(depth: Int = 4): String = {
    val stackTrace = new Exception().getStackTrace()
    val trace = stackTrace.apply(depth)
    val location = new TraceLocation(trace)
    tryOr {
      val content = readLines(srcDir+location.path)
      content(location.lineNumber - 1)
    } (e => "No source file found at "+srcDir+location.path)
  }

  def location = {
    val stackTrace = new Exception().getStackTrace().toList
    val trace = stackTrace.filterNot(_.toString.contains("org.specs2")).headOption
    new TraceLocation(trace.getOrElse(stackTrace(0)))
  }
}
object FromSource extends FromSource