package org.specs2
package specification
package process

import control.{Actions, Action}
import execute.Result
import scalaz.std.anyVal._
import scalaz.syntax.bind._
import java.io._
import scalaz.stream.io._
import specification.core.Description
import scalaz.stream.{process1, Process}
import scala.collection.mutable.HashMap


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

trait Store {
  def get[A](key: Key[A]): Action[Option[A]]
  def set[A](key: Key[A], a: A): Action[Unit]
  def reset: Action[Unit]
}

object Store {
  def file(baseDirectory: String) = new Store {

    def set[A](key: Key[A], fact: A): Action[Unit] =
      Actions.fromIO(filepath(key).getParentFile.mkdirs) >>
      Actions.fromTask {
        Process(StoreKeys.encode(key, fact)).toSource.pipe(process1.utf8Encode).to(fileChunkW(filepath(key).toString)).run
      }

    def get[A](key: Key[A]): Action[Option[A]] =
      if (filepath(key).exists)
        Actions.fromTask {
          linesR(filepath(key).toString)
            .reduce((l1: String, l2: String) => l1+"\n"+l2).map(s => StoreKeys.decode(key, s)).runLog.map(_.headOption.flatten)
        }
      else Actions.ok(None)

    private def filepath[A](key: Key[A]): File =
      new File(s"""$baseDirectory/${StoreKeys.resolve(key)}""")

    def reset: Action[Unit] =
      Actions.fromIO {
        new File(baseDirectory).listFiles.map(_.delete)
        new File(baseDirectory).delete
      }
  }

  val memory = MemoryStore()
}

case class MemoryStore(statistics: HashMap[String, Stats] = new HashMap[String, Stats],
                       results: HashMap[(String, Long), Result] = new HashMap[(String, Long), Result]) extends Store {
  def get[A](key: Key[A]): Action[Option[A]] = key match {
    case SpecificationStatsKey(specClassName) => Actions.ok(statistics.get(specClassName))
    case SpecificationResultKey(specClassName, description) => Actions.ok(results.get((specClassName, description.hashCode)))
  }
  def set[A](key: Key[A], a: A): Action[Unit] = key match {
    case SpecificationStatsKey(specClassName) => Actions.ok(statistics.put(specClassName, a))
    case SpecificationResultKey(specClassName, description) => Actions.ok(results.put((specClassName, description.hashCode), a))
  }

  def reset: Action[Unit] = Actions.ok({
    statistics.clear
    results.clear
  })

}

sealed trait Key[A]
case class SpecificationStatsKey(specClassName: String) extends Key[Stats]
case class SpecificationResultKey(specClassName: String, description: Description) extends Key[Result]

object StatisticsRepository {
  val memory = StatisticsRepository(Store.memory)
  def file(path: String) = StatisticsRepository(Store.file(path))
}
