package org.specs2
package io
import java.io.File
import specification._

class FileWriterSpec extends Specification {
  val examples = 
  "A FileWriter should"^
    "write inside a file" ! c(e1)^
    "close the file if an exception occurs and rethrow the exception" ! c(e2)

  object c extends AfterContext {
	def after = new File("filePath").delete	
  }
  def e1 = {
	fw.write("filePath")(_.write("hello world"))
    out.messages must_== List("hello world")
  }
  def e2 = {
	try { fw.write("filePath")(_ => error("bad")) }
    catch { case e => { e.getMessage must_== "bad"} }
    out.closed must_== true
  }

  object fw extends FileWriter {
    override def getWriter(path: String) = out
  }
  object out extends MockWriter
}