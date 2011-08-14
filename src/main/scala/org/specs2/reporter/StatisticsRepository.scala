package org.specs2
package reporter

import scala.xml._
import io.FileSystem
import xml.Nodex._
import specification.{Example, ExecutedResult, Stats, SpecName}
import execute.{Result, Success}

private[specs2]
trait StatisticsRepository {
  def getStatistics(specName: SpecName): Option[Stats]
  def storeStatistics(specName: SpecName, stats: Stats): this.type
  def storeResult(specName: SpecName, result: ExecutedResult): this.type

  /**
   * remove previously stored statistics
   */
  def resetStatistics: this.type
  /**
   * @return the previous executed result of an example
   */
  def previousResult(specName: SpecName, e: Example): Option[Result]
}

/**
 * This repository store the results in one file per specification, in a special directory
 *
 * This solution is preferred over having a single file for all specifications because of the possible
 * concurrent execution of specifications
 */
private[specs2]
trait DefaultStatisticsRepository extends StatisticsRepository with OutputDir {

  protected lazy val statsDirName = "stats/"
  protected lazy val statsDirPath = outputDir + statsDirName

  /**
   * @return the latest statistics for a given specification
   */
  def getStatistics(specName: SpecName): Option[Stats] = {
    (loadStatistics(specName) \\ nameTag(specName)).lastOption.flatMap(_.child.headOption) flatMap Stats.fromXml
  }

  /**
   * reset the statistics
   */
  def resetStatistics = {
    fileWriter.delete(statsDirPath)
    this
  }
  /**
   * @return the previous executed result of an example
   */
  def previousResult(specName: SpecName, e: Example) = Some(Success())

  def loadStatistics(specName: SpecName): NodeSeq = fileSystem.loadXhtmlFile(specStatsPath(specName))

  def storeResult(specName: SpecName, result: ExecutedResult) = this

  def storeStatistics(specName: SpecName, stats: Stats) = {
    fileWriter.appendToXmlFile(specStatsPath(specName), toXml(specName, stats))
    this
  }

  def specStatsPath(specName: SpecName) = statsDirPath + specName.fullName + ".stats"

  /**
   * make sure that no empty tag name is used to search the xml stats and replace . with : to help the xpath search
   */
  private def nameTag(specName: SpecName) = if (specName.fullName.isEmpty) "anon" else specName.fullName.replace(".", ":")

  private def toXml(specName: SpecName, stats: Stats) = {
    Elem(null, nameTag(specName),
         new UnprefixedAttribute("timestamp", System.currentTimeMillis().toString, Null), TopScope, stats.toXml) ++ Text("\n")
  }
}
private[specs2]
object DefaultStatisticsRepository extends DefaultStatisticsRepository

private[specs2]
trait WithStatisticsRepository {
  protected def repository: StatisticsRepository
}
private[specs2]
trait WithDefaultStatisticsRepository extends WithStatisticsRepository {
  protected lazy val repository: StatisticsRepository = DefaultStatisticsRepository
}
