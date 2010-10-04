package org.specs2
package runner
import mock.Mockito
import _root_.org.scalatools.testing._

class TestInterfaceRunnerSpec extends SpecificationWithJUnit with Mockito {
  val examples = 
"""
  A TestInterfaceRunner is responsible for instantiating Specification classes found by sbt
  and executing them using a TestInterfaceReporter
  
"""^
" if the specification instance cannot be created, there must be an error logged" ! missing().e1^
" if the specification instance cannot be created, a stacktrace must be logged" ! missing().e2^
end

  case class missing() {
	val logger = mock[Logger]
	val runner = new TestInterfaceRunner(getClass.getClassLoader, Array(logger))
	runner.run("missing", mock[TestFingerprint], mock[EventHandler], Array(""))
	
	def e1 = there was one(logger).error("Could not create an instance of missing\n")
	def e2 = there was one(logger).error("Could not create an instance of missing\n") then 
	                   one(logger).error("  java.lang.ClassNotFoundException: missing\n")
  }
}