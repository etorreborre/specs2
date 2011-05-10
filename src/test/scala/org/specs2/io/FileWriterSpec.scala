package org.specs2
package io
import java.io.File
import specification._
import execute._
import sys._

class FileWriterSpec extends Specification {  def is =

  "A FileWriter should"                                                                                                 ^
    "write inside a file"                                                                                               ! c().e1^
    "close the file if an exception occurs"                                                                             ! c().e2^
    "rethrow the exception if an exception occurs"                                                                      ! c().e3

  case class c() extends After {
    val out = new MockWriter {}
    val fw = new  FileWriter { override def getWriter(path: String) = out  }

    def e1 = this {
      fw.write("filePath")(_.write("hello world"))
      out.messages must_== List("hello world")
    }
    def e2 = this {
      try { fw.write("filePath")(_ => error("bad")) }
      catch { case e => () }
      out.closed must_== true
    }
    def e3 = this {
      try { fw.write("filePath")(_ => error("bad")); Failure("an exception must be thrown") }
      catch { case e => { e.getMessage must_== "bad" }.toResult }
    }
    def after = { new File("filePath").delete }
  }
}