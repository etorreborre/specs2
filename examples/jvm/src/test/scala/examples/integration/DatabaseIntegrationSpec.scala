package examples.integration

import org.specs2.*
import scala.concurrent.ExecutionContext

class DatabaseIntegrationSpec(implicit ec: ExecutionContext) extends Specification, StartDatabase:
  def is = s2"""

  First database integration test $e1
  Second database integration test $e2

"""

  def e1 = (db: Database) => ok
  def e2 = (db: Database) => ok
