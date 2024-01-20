package examples.integration

import org.specs2.*
import org.specs2.specification.core.*

class RemoteServiceIntegrationSpec(env: Env) extends Specification, StartDatabase(env):
  def is = s2"""

  First remote service integration test $e1
  Second remote service integration test $e2

"""

  def e1 = (db: Database) => ok
  def e2 = (db: Database) => ok
