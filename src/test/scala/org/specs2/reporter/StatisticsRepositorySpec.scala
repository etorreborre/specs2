package org.specs2
package reporter
import specification._

class StatisticsRepositorySpec extends Specification with DefaultStatisticsRepository { def is = sequential ^
                                                                              p^
  "A statistics repository can"                                               ^
    "store statistics per specification name"                                 ! withStatsFile(e1)^
    "each store action store statistics with a new timestamp"                 ! withStatsFile(e2)^
    "retrieve statistics per specification name"                              ! withStatsFile(e3)^
    "the retrieval gets the latest statistics"                                ! withStatsFile(e4)^
                                                                              end


  object withStatsFile extends After {
    def after = new java.io.File(statsFilePath).delete
  }

  def e1 = pending
  def e2 = pending
  def e3 = pending
  def e4 = pending
}