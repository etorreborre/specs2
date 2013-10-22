package org.specs2
package reporter

import specification._
import mock._

class HtmlPrinterSpec extends Specification with Mockito { outer => def is = s2"""

#### Introduction

The HtmlPrinter class is responsible for creating html files from an executed specifications:

                           print

      ExecutedSpecification  =>  Seq[HtmlFile]

This actually works in 4 steps:

                            reduce            sortByFile             createToc                   printHtml

      ExecutedSpecification  => Seq[HtmlLine]    => Tree[HtmlFileLines]  =>  TreeToc                =>  Seq[HtmlFile]
                                                                         =>  new HtmlReportOutput
                                                                         =>  Seq[HtmlFileLines]


#### Reduction

 The executed specification fragments are mapped to `HtmlLine` objects
 ${"reduction" ~/ new HtmlLinesSpec}

#### Sorting by file

 ${"The HtmlLine objects are aggregrated to a Tree of HtmlFileLines" ~/ new HtmlFileLinesSpec}

#### Creating the table of contents

 ${"The tree of HtmlFileLine are used to create a table of contents for the whole specification" ~/ new HtmlTocSpec}

#### Printing the lines as xhtml

 ${"The HtmlFileLines objects are used to create xhtml (HtmlFile) to be written to files" ~/ new HtmlFileSpec}
                                                                                                                        """
}

