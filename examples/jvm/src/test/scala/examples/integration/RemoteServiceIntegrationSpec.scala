package examples.integration

import org.specs2.*
import scala.concurrent.ExecutionContext

class RemoteServiceIntegrationSpec(implicit ec: ExecutionContext) extends Specification, StartDatabase:
  def is = s2"""

  First remote service integration test $e1
  Second remote service integration test $e2

"""

  def e1 = (db: Database) => ok
  def e2 = (db: Database) => ok
