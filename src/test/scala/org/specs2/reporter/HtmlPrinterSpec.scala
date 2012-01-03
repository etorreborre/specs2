package org.specs2
package reporter
import mock._
import text.MarkdownHeaders._

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
  h4> "Reduction"                                                                                                               ^
    "The executed specification fragments are mapped to `HtmlLine` objects"                                                     ^
     "reduction" ~/(new HtmlLinesSpec)                                                                                          ^
                                                                                                                                p^
  h4> "Sorting by file"                                                                                                         ^
      "The HtmlLine objects are aggregrated to a Tree of HtmlFileLines" ~/(new HtmlFileLinesSpec)                               ^
                                                                                                                                p^
  h4> "Creating the table of contents"                                                                                          ^
      "The tree of HtmlFileLine must be used to create a table of contents for the whole specification" ~/(new HtmlTocSpec)     ^
                                                                                                                                p^
  h4> "Printing the lines as xhtml"                                                                                             ^
      "The HtmlFileLines must be created as xhtml to be written to files" ~/(new HtmlFileSpec)                                  ^
                                                                                                                                end
}

