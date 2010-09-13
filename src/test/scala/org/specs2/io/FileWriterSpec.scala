package org.specs2
package io
import java.io.File
import specification._
import execute._

class FileWriterSpec extends Specification {
  val examples = 
  "A FileWriter should"^
    "write inside a file" ! c(e1)^
    "close the file if an exception occurs" ! c(e2)^
    "rethrow the exception if an exception occurs" ! c(e3)

  def e1 = {
	fw.write("filePath")(_.write("hello world"))
    out.messages must_== List("hello world")
  }
  def e2 = {
	try { fw.write("filePath")(_ => error("bad")) }
    catch { case e => () }
    out.closed must_== true
  }
  def e3: Result = {
	try { fw.write("filePath")(_ => error("bad")); Failure("an exception must be thrown") }
    catch { case e => { e.getMessage must_== "bad"} }
  }
  object c extends After {
	def after = new File("filePath").delete	
  }
  object fw extends FileWriter {
    override def getWriter(path: String) = out
  }
  object out extends MockWriter
}