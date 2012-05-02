package org.specs2
package reporter

import specification._
import ExecutedSpecificationData._

class HtmlFileLinesSpec extends Specification { def is =

  "A linked specification must create a new file"                                                                       ! e1^
    "that file must be linked to the parent file"                                                                       ! e2^
  "A see specification must not create a new file but just a link"                                                      ! e3^
  "The user can specify a different path for the html file"                                                             ! e4^
                                                                                                                        end

  lazy val spec1: Fragments = "ex1" ! failure ^ "a " ~  ("successfull spec", successfulSubSpec) ^ end
  lazy val spec2: Fragments = "ex1" ! failure ^ "a " ~/ ("successfull spec", successfulSubSpec) ^ end
  lazy val spec3: Fragments = "title".title(filePath="different.html") ^ "ex1" ! ok

  lazy val successfulSubSpec = new Specification { def is = "ex1" ! success }

  def e1 = htmlFileLines(spec1).flatten must have size(2)
  def e2 = htmlFileLines(spec1).subForest must have size(1)
  def e3 = htmlFileLines(spec2).flatten must have size(1)
  def e4 = htmlFileLines(spec3).flatten.head.link.url must_== "different.html"

  def printer = new HtmlPrinter { }
  def htmlFileLines(spec: Fragments) = printer.createHtmlLinesFiles(execute(spec))

}