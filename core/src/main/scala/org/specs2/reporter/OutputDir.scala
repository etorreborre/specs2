package org.specs2
package reporter

import io._
import main.SystemProperties
import Paths._

private[specs2]
trait OutputDir {
  /** the file system is used to open the file to write */
  private[specs2] lazy val fileSystem = new FileSystem {}
  /** the file writer is used to open the file to write */
  private[specs2] lazy val fileWriter = new FileWriter {}

  /**
   * the output directory is either defined by a specs2 system variable
   * or chosen as a reports directory in the standard maven "target" directory
   */
  private[specs2] lazy val outputDir: String = SystemProperties.getOrElse("outDir", "target/specs2-reports/").absoluteDirPath

  /**
   * the statistics directory is either defined by a specs2 system variable
   * or defined as a subdirectory of the output directory
   */
  private[specs2] lazy val statsDirPath: String = SystemProperties.getOrElse("statsDir", outputDir + statsDirName).absoluteDirPath

  private[specs2] lazy val statsDirName: String = "stats/"

  /** @return the file path for the html output */
  def reportPath(url: String) = outputDir + url
}