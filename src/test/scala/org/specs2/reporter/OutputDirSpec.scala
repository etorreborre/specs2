package org.specs2
package reporter

import specification.SpecName._


class OutputDirSpec extends Specification { def is =
                                                                                                                                            """
The OutputDir trait is responsible for managing the output files location.
                                                                                                                                            """^p^
  "The report path must"                                                                                                                      ^
    "be target/specs-reports as a default value"                                                                                            ! filepath().e1^
    "use the `outDir` system variable if set"                                                                                               ! filepath().e2^
                                                                                                                                            end

  implicit val argument = args()

  case class filepath() {
    def e1 = outputDir.reportPath("") must endWith("target/specs2-reports/")
    def e2 = new OutputDir { override lazy val outputDir = "output/" }.reportPath("") must
             startWith("output/")
  }

  val outputDir = new OutputDir {}

}