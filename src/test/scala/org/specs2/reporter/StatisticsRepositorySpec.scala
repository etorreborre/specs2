package org.specs2
package reporter
import specification._

class StatisticsRepositorySpec extends Specification { def is = sequential ^
                                                                                                            p^
  "A statistics repository can"                                                                             ^
    "store / retrieve statistics per specification name"                                                    ! repo().e1^
    "store statistics with a new timestamp so that the retrieval gets the latest statistics"                ! repo().e2^
                                                                                                            end


  case class repo() extends DefaultStatisticsRepository with BeforeAfter {
    // for the following examples, write the results to a different stats file to avoid that file being overwritten
    // by the StatisticsRepositorySpec run
    override lazy val statsFileName = "testspecs2.stats"

    def before = new java.io.File(statsFilePath).delete
    def after = before
    val specName1 = SpecName("spec1")
    val specName2 = SpecName("spec2")
    val (stats1, stats2) = (Stats(failures = 3), Stats(failures = 4))

    def e1 = this {
      storeStatistics(specName1, stats1)
      storeStatistics(specName2, stats2)
      getStatistics(specName1) must_== Some(Stats(failures = 3))
    }

    def e2 = this {
      storeStatistics(specName1, stats1)
      storeStatistics(specName1, stats2)
      getStatistics(specName1) must_== Some(Stats(failures = 4))
    }
  }
}