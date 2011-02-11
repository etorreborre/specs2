package org.specs2
package tools

import java.io.{ PrintWriter, StringWriter, File }
import java.net.{ URLClassLoader, URLDecoder }
import _root_.scala.tools.nsc.{ Interpreter, Settings, InterpreterResults }
import text.Trim._

/**
 * Implicit for interpreting a String as a Scala script
 */
trait ScalaInterpreter {
  implicit def script(s:String) = Script(s)
}
case class Script(script: String) {
  lazy val lines = script.nonEmptyLines

  def interpret = {
    val writer = new StringWriter
    val interpreter = createInterpreter(writer)
    lines.foldLeft(InterpreterResults.Success: InterpreterResults.Result) { (result, line) =>
      result match {
        case InterpreterResults.Incomplete | InterpreterResults.Error => result
        case InterpreterResults.Success => interpreter.interpret(line)
      }
    }
    if (Seq("error", "at ").contains(writer)) writer.removeEmptyLines
    else                                      writer.lastBlock
  }

  private def createInterpreter(writer: StringWriter) = {
    val cl = Thread.currentThread.getContextClassLoader
    val paths = if (cl.isInstanceOf[URLClassLoader]) {
      cl.asInstanceOf[URLClassLoader].getURLs.toList.collect {
        case url if url.getProtocol == "file" => new File(URLDecoder.decode(url.getPath, "UTF-8")).getCanonicalFile.getAbsolutePath
      }
    } else Nil
    val settings  = {
      val s = new Settings
      s.classpath.value = (s.classpath.value :: paths).mkString(File.pathSeparator)
      s
    }
    new Interpreter(settings, new PrintWriter(writer))
  }
}
