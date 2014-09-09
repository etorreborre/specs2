package org.specs2
package control

import io.FilePath
import scalaz.std.anyVal._
import scala.sys.process.ProcessLogger

/**
 * Execute external commands
 */
object Executable {
  /**
   * Run an external program
   */
  def run(executable: FilePath, arguments: Seq[String] = Seq()): Action[Unit] = {
    val logger = new StringProcessLogger
    try {

      val code = sys.process.Process(executable.path, arguments).!(logger)
      if (code == 0) Actions.ok(())
      else           Actions.fail(logger.lines)
    } catch { case t: Throwable =>
      Actions.fail(t.getMessage+"\n"+logger.lines)
    }
  }

  val NullProcessLogger = new ProcessLogger {
    def buffer[T](f: => T): T = f
    def err(s: => String) {}
    def out(s: => String) {}
  }

  def stringProcessLogger = new StringProcessLogger
  class StringProcessLogger extends ProcessLogger {
    private val messages = new StringBuilder
    def lines = messages.toString

    def buffer[T](f: => T): T = {
      messages.clear
      f
    }
    def err(s: => String) { messages.append(s+"\n") }
    def out(s: => String) { messages.append(s+"\n") }
  }


}
