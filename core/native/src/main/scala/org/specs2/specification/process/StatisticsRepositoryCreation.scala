package org.specs2
package specification
package process

import io._

object StatisticsRepositoryCreation {

  def memory = StatisticsRepository(StatisticsMemoryStore())
  def file(dir: DirectoryPath) = StatisticsRepository(DirectoryStore(dir))
  
}

