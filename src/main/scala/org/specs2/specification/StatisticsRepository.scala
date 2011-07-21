package org.specs2
package specification


trait StatisticsRepository {
  def statsFrom(name: SpecName): Stats
  def store(name: SpecName, stats: Stats): this.type
}

class DefaultStatisticsRepository extends StatisticsRepository {
  def statsFrom(name: SpecName): Stats = Stats()
}