package org.specs2
package specification
package process

import io._
import control._

object StatisticsRepositoryCreation {

  def memory = DefaultStatisticsRepository(StatisticsMemoryStore())
  def file(dir: DirectoryPath) = DefaultStatisticsRepository(DirectoryStore(dir, FileSystem(ConsoleLogger())))

}
