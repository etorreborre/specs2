package org.specs2
package specification


trait StatisticsRepository {
  def statsFrom(fragment: ExecutedFragment): Stats
  def store(fragment: ExecutedFragment, stats: Stats): this.type
}

class DefaultStatisticsRepository extends StatisticsRepository {
  def statsFrom(fragment: ExecutedFragment): Stats = Stats()
  def store(fragment: ExecutedFragment, stats: Stats) = this
}