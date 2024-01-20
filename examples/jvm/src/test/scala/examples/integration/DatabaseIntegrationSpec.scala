package examples.integration

import org.specs2.*
import org.specs2.specification.core.*

class DatabaseIntegrationSpec(env: Env) extends Specification, StartDatabase(env):
  def is = s2"""

  First database integration test $e1
  Second database integration test $e2

"""

  def e1 = (db: Database) => ok
  def e2 = (db: Database) => ok
