package org.specs2
package control

import io.FilePath
import scala.sys.process.ProcessLogger
import Operations._
import fp.syntax._

/**
 * Execute external commands
 */
object Executable {

  /**
   * Run an external program
   */
  def run(executable: FilePath, arguments: Seq[String] = Seq()): Operation[Unit] =
    execute(executable, arguments).void

  /**
   * Execute an external program and return the output
   */
  def execute(executable: FilePath, arguments: Seq[String] = Seq()): Operation[String] = {
    lazy val logger = new StringProcessLogger
    attempt {
      protect(sys.process.Process(executable.path, arguments).!(logger)).flatMap { code =>
        if (code == 0) ok(logger.lines)
        else           fail[String](logger.lines)
      }
    }.flatMap {
       case Left(t)  => fail[String](t.getMessage+"\n"+logger.lines)
       case Right(s) => ok(s)
    }
  }

  val NullProcessLogger = new ProcessLogger {
    def buffer[T](f: => T): T = f
    def err(s: => String): Unit = {}
    def out(s: => String): Unit = {}
  }

  def stringProcessLogger = new StringProcessLogger
  class StringProcessLogger extends ProcessLogger {
    private val messages = new StringBuilder
    def lines = messages.toString

    def buffer[T](f: => T): T = {
      messages.clear
      f
    }
    def err(s: => String): Unit = { messages.append(s+"\n"); () }
    def out(s: => String): Unit = { messages.append(s+"\n"); () }
  }


}
