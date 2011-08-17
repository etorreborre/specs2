package org.specs2
package reporter

import internal.scalaz.Scalaz._
import scala.xml._
import xml.Nodex._
import specification.{Example, ExecutedResult, Stats, SpecName}
import execute.{Result, Success}
import io.{Location, FileSystem}

private[specs2]
trait StatisticsRepository {
  def getStatistics(specName: SpecName): Option[Stats]
  def storeStatistics(specName: SpecName, stats: Stats): this.type
  def storeResults(specName: SpecName, result: Seq[ExecutedResult]): this.type

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
  def getStatistics(specName: SpecName): Option[Stats] = latestSpecStats(specName) |> extractStats

  /**
   * @return the latest saved statistics of a given specification
   */
  def latestSpecStats(specName: SpecName): Option[Node] = (loadStatistics(specName) \\ statsTag(specName)).lastOption
  /**
   * @return the latest saved results of a given specification
   */
  def latestSpecResults(specName: SpecName): Option[Node] = (loadStatistics(specName) \\ resultsTag(specName)).lastOption

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
  def previousResult(specName: SpecName, e: Example): Option[Result] = latestSpecResults(specName).getOrElse(NodeSeq.Empty) |> findPreviousStats(e)

  /**
   * find the pr of an example in the loaded xml
   */
  def findPreviousStats(e: Example) = (n: NodeSeq) => {
   (n \\ "_" find attributeValueEquals(exampleId(e))) |>
     extractStats map (_.result) orElse
     Some(Success())
  }

  private def extractStats = (n: Option[Node]) => n flatMap (_.child.headOption) flatMap Stats.fromXml

  private def attributeValueEquals(value: String)(node: Node) = node.attributes.exists(_.value.toString == value)


  def loadStatistics(specName: SpecName): NodeSeq = synchronized { fileSystem.loadXhtmlFile(specStatsPath(specName)) }

  def storeResults(specName: SpecName, results: Seq[ExecutedResult]) = {
    fileWriter.appendToXmlFile(specStatsPath(specName), resultsToXml(specName, results))
    loadStatistics(specName)
    this
  }

  def storeStatistics(specName: SpecName, stats: Stats) = {
    fileWriter.appendToXmlFile(specStatsPath(specName), statsToXml(specName, stats))
    this
  }

  def specStatsPath(specName: SpecName) = statsDirPath + specName.fullName + ".stats"

  /**
   * make sure that no empty tag name is used to search the xml stats and replace . with : to help the xpath search
   */
  private def nameTag(specName: SpecName) = if (specName.fullName.isEmpty) "anon-"+specName.javaClassName.hashCode else specName.fullName

  private def statsTag(specName: SpecName) = nameTag(specName)+"-stats"
  private def resultsTag(specName: SpecName) = nameTag(specName)+"-results"

  private def statsToXml(specName: SpecName, stats: Stats): NodeSeq = {
    Elem(null, statsTag(specName),
         new UnprefixedAttribute("timestamp", System.currentTimeMillis().toString, Null), TopScope, stats.toXml) ++ Text("\n")
  }

  private def resultsToXml(specName: SpecName, results: Seq[ExecutedResult]): NodeSeq = {
    val xmlResults = results reduceNodes resultToXml
    if (xmlResults.isEmpty)  xmlResults
    else                     Elem(null, resultsTag(specName),
                               new UnprefixedAttribute("timestamp", System.currentTimeMillis().toString, Null), TopScope, xmlResults:_*) ++ Text("\n")
  }

  /**
   * Transform a result to xml for storage. Only non-successes are stored. The results are referenced with a hashcode based on the result description
   * Location are stored for easier reference
   */
  private def resultToXml: ExecutedResult => NodeSeq = (r: ExecutedResult) => {
    (<result location={r.location.toString} id={resultId(r)}>{r.stats.toXml}</result> ++ Text("\n")) unless r.isSuccess
  }

  private def resultId(r: ExecutedResult): String = r.s.toString.hashCode.toString
  private def exampleId(e: Example): String = e.desc.toString.hashCode.toString


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
