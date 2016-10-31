package org.specs2
package specification
package process

import control._
import execute.Result
import io._
import specification.core.Description
import scala.collection.mutable.HashMap

/**
 * Store the execution statistics.
 *
 * The actual store might be on disk on in-memory
 */
case class StatisticsRepository(store: Store) {
  /** get the latest statistics for a given specification */
  def getStatistics(specClassName: String): Action[Option[Stats]] =
    store.get(SpecificationStatsKey(specClassName))

  /** get the latest statistics for a given specification, or return a default value */
  def getStatisticsOr(specClassName: String, stats: Stats): Action[Stats] =
    getStatistics(specClassName).map(_.getOrElse(stats))

  /** store the final statistics for a given specification */
  def storeStatistics(specClassName: String, stats: Stats): Action[Unit] =
    store.set(SpecificationStatsKey(specClassName), stats)

  def storeResult(specClassName: String, description: Description, result: Result): Action[Unit] =
    store.set(SpecificationResultKey(specClassName, description), result)

  /** @return the previous executed result of an example */
  def previousResult(specClassName: String, d: Description): Action[Option[Result]] =
    store.get(SpecificationResultKey(specClassName, d))

  /** remove all previously stored statistics */
  def resetStatistics: Action[Unit] =
    store.reset
}

/**
 * In memory store for statistics
 */
case class StatisticsMemoryStore(statistics: HashMap[String, Stats] = new HashMap[String, Stats],
                       results: HashMap[(String, Long), Result] = new HashMap[(String, Long), Result]) extends Store {
  def get[A](key: Key[A]): Action[Option[A]] = key match {
    case SpecificationStatsKey(specClassName) =>
      Actions.ok(statistics.get(specClassName))

    case SpecificationResultKey(specClassName, description) =>
      Actions.ok(results.get((specClassName, description.hashCode.toLong)))
  }

  def set[A](key: Key[A], a: A): Action[Unit] = key match {
    case SpecificationStatsKey(specClassName) =>
      Actions.ok(statistics.put(specClassName, a)).map(_ => ())

    case SpecificationResultKey(specClassName, description) =>
      Actions.ok(results.put((specClassName, description.hashCode.toLong), a)).map(_ => ())
  }

  def reset: Action[Unit] = Actions.ok {
    statistics.clear
    results.clear
  }

}

case class SpecificationStatsKey(specClassName: String) extends Key[Stats]
case class SpecificationResultKey(specClassName: String, description: Description) extends Key[Result]

object StatisticsRepository {
  def memory = StatisticsRepository(StatisticsStore.memory)
  def file(dir: DirectoryPath) = StatisticsRepository(StatisticsStore.directory(dir))
}

object StatisticsStore {
  import FileSystem._

  /**
   * key-value store backed-up by the file system
   */
  def directory(baseDirectory: DirectoryPath) = new Store {

    def set[A](key: Key[A], fact: A): Action[Unit] =
      writeFile(filepath(key), StoreKeys.encode(key, fact))

    def get[A](key: Key[A]): Action[Option[A]] =
      exists(filepath(key)).toAction.flatMap { e =>
        if (e) readFile(filepath(key)).map(content => StoreKeys.decode(key, content)).toAction
        else   Actions.ok(None)
      }

    def reset: Action[Unit] = delete(baseDirectory)

    private def filepath[A](key: Key[A]): FilePath =
      baseDirectory / FilePath.unsafe(StoreKeys.resolve(key))
  }

  def memory = StatisticsMemoryStore()
}
