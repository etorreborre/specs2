package examples.integration

import org.specs2.Specification
import org.specs2.specification.Resource
import org.specs2.specification.core.*
import org.specs2.control.Ref
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.*

case class Database(isStarted: Boolean = false):
  def start: Database =
    copy(isStarted = true)

  def shutdown: Database =
    copy(isStarted = false)

trait StartDatabase(env: Env) extends Resource[Database]:
  given ExecutionContext = env.executionContext

  override def resourceKey: Option[String] =
    Some("shared database")

  def acquire: Future[Database] =
    Future.successful(Database().start)

  def release(db: Database): Execution =
    Future { db.shutdown; true }
