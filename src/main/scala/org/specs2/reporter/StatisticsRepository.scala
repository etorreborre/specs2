package org.specs2
package reporter

import scala.xml._
import io.FileSystem
import xml.Nodex._
import specification.{ExecutedFragment, Stats, SpecName}

private[specs2]
trait StatisticsRepository {
  def getStatistics(specName: SpecName): Option[Stats]
  def storeStatistics(specName: SpecName, stats: Stats): this.type
}

private[specs2]
trait DefaultStatisticsRepository extends StatisticsRepository with OutputDir {

  protected lazy val statsFileName = "specs2.stats"
  protected lazy val statsFilePath = outputDir + statsFileName

  lazy val allStats = fileSystem.loadXhtmlFile(statsFilePath)

  /**
   * @return the latest statistics for a given specification
   */
  def getStatistics(specName: SpecName): Option[Stats] = {
    val allSpecs = (allStats \\ nameTag(specName))
    val specStats = allSpecs.lastOption.flatMap(_.child.headOption).getOrElse(<none/>)
    Stats.fromXml(specStats)
  }

  def storeStatistics(specName: SpecName, stats: Stats) = {
    fileWriter.appendToXmlFile(statsFilePath, toXml(specName, stats))
    this
  }

  private def nameTag(specName: SpecName) = specName.fullName.replace(".", ":")

  private def toXml(specName: SpecName, stats: Stats) = {
    Elem(null, nameTag(specName),
         new UnprefixedAttribute("timestamp", System.currentTimeMillis().toString, Null), TopScope, stats.toXml) ++ Text("\n")
  }
}
private[specs2]
object DefaultStatisticsRepository extends DefaultStatisticsRepository


