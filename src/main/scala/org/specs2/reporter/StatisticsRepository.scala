package org.specs2
package reporter

import scala.xml._
import io.FileSystem
import specification.{ExecutedFragment, Stats, SpecName}

private[specs2]
trait StatisticsRepository {
  def getStatistics(specName: SpecName): Stats
  def storeStatistics(specName: SpecName, stats: Stats): this.type
}

private[specs2]
class DefaultStatisticsRepository extends StatisticsRepository with OutputDir {

  private val statsFilePath = outputDir + "specs2.stats"

  lazy val allStats = fileSystem.loadXhtmlFile(statsFilePath)

  def getStatistics(specName: SpecName): Stats = Stats.fromXml((allStats \\ specName.name).lastOption.getOrElse(<none/>))

  def storeStatistics(specName: SpecName, stats: Stats) = {
    fileWriter.appendToXmlFile(statsFilePath, toXml(specName, stats))
    this
  }

  private def toXml(specName: SpecName, stats: Stats) = {
    Elem(null, specName.fullName, null, TopScope, stats.toXml)
  }
}


