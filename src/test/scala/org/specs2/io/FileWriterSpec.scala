package org.specs2
package io

import java.io.File
import specification._
import sys._
import control.Exceptions._

class FileWriterSpec extends script.Specification with Groups { def is = s2"""

  A FileWriter should                                                                                                 
    + write inside a file
    + close the file if an exception occurs
    + rethrow the exception if an exception occurs

  A FileWriter can
    + write a XML Node
                                                                                                        """

  "file writer" - new g1 with After {
    val out = new MockWriter {}
    val fw = new  FileWriter { override def getWriter(path: String, append: Boolean = false) = out  }

    e1 := this {
      fw.write("filePath")(_.write("hello world"))
      out.messages must_== List("hello world")
    }

    e2 := this {
      tryOk { fw.write("filePath")(_ => error("bad")) }
      out.closed must_== true
    }

    e3 := this { fw.write("filePath")(_ => error("bad")) must throwAn[Exception](message = "bad") }

    e4 := this {
      fw.writeXmlFile("filePath", <hello/>)
      out.messages must contain("<hello></hello>")
    }

    def after { new File("filePath").delete }
  }
}