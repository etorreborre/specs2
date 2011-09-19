package org.specs2
package reporter
import io._
import mock._
import specification._
import text.MarkdownHeaders._
import matcher.DataTables
import specification.HtmlLink._

class HtmlPrinterSpec extends SpecificationWithJUnit with Mockito { outer => def is =
  h4> "Introduction"^
                                                                                                                        """
The HtmlPrinter class is responsible for opening an html file and writing the specification text:

                           print

      ExecutedSpecification  =>  Seq[HtmlFile]

This actually works in 4 steps:

                            reduce            sortByFile             createToc                   printHtml

      ExecutedSpecification  => Seq[HtmlLine]    => Tree[HtmlFileLines]  =>  TreeToc                =>  Seq[HtmlFile]
                                                                         =>  new HtmlReportOutput
                                                                         =>  Seq[HtmlFileLines]
                                                                                                                        """^
                                                                                                                        p^
  h4> "Reduction"                                                                                                       ^
    "The executed specification fragments are mapped to `HtmlLine` objects"                                             ^
     "reduction" ~/(new HtmlLinesSpec)                                                                                  ^
                                                                                                                        p^
  h4> "Sorting by file"                                                                                                 ^
      "The HtmlLine objects are aggregrated to a Tree of HtmlFileLines" ~/(new HtmlFileLinesSpec)                       ^
                                                                                                                        end
}

