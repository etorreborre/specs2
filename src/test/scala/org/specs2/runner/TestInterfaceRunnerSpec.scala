package org.specs2
package runner
import mock.Mockito
import io._
import _root_.org.scalatools.testing._
import reporter._

class TestInterfaceRunnerSpec extends SpecificationWithJUnit with Mockito {
  val content = 
"""
  A TestInterfaceRunner is responsible for instantiating Specification classes found by sbt
  and executing them using a TestInterfaceReporter
  
"""^
" if the specification class is missing"^
"   there must be an error logged" ! missing().e1^
"   a stacktrace must be logged" ! missing().e2^
p^
" if the specification instance cannot be created"^
"   a stacktrace for the exception must be logged" ! instance().e1^
"   the cause stacktrace must also be logged if there is one" ! instance().e2^
"   the cause stacktrace must be nicely separated from the top exception" ! instance().e3^
end^
" if the specification instance can be created it must be passed to TestInterface reporter" ! reporter().e1^
end

  case class missing() extends MockLogger {
	val runner = new TestInterfaceRunner(getClass.getClassLoader, Array(logger))
	runner.run("missing", mock[TestFingerprint], mock[EventHandler], Array(""))
	
	def e1 = logger.messages must contain("error: Could not create an instance of missing\n")
	def e2 = logger.messages must contain("error:   caused by java.lang.ClassNotFoundException: missing")
  }

  case class instance() extends MockLogger {
	val runner = new TestInterfaceRunner(getClass.getClassLoader, Array(logger))
	runner.run("org.specs2.runner.SpecificationForSbtWithException", mock[TestFingerprint], mock[EventHandler], Array(""))
	
	def e1 = logger.messages must contain("error: Could not create an instance of org.specs2.runner.SpecificationForSbtWithException\n")
	def e2 = logger.messages must contain("error:   caused by java.lang.reflect.InvocationTargetException")
	def e3 = logger.messages must contain("error:   caused by java.lang.Exception: fail")
  }

  case class reporter() extends MockLogger { outer =>
	val reporter = mock[TestInterfaceReporter]
	val runner = new TestInterfaceRunner(getClass.getClassLoader, Array(logger)) {
	  override def reporter(handler: EventHandler) = outer.reporter
	}
	runner.run("org.specs2.runner.SpecificationForSbt", mock[TestFingerprint], mock[EventHandler], Array(""))
	
	def e1 = there was one(reporter).report(any[Specification])
  }
}

trait MockLogger {
  val logger = new Logger with MockOutput {
	override def ansiCodesSupported = false
	override def error(message: String) = println("error: " + message)
	override def info(message: String) = println("info: " + message)
	override def warn(message: String) = println("warn: " + message)
	override def debug(message: String) = println("debug: " + message)
	override def trace(t: Throwable) = println("trace: " + t)
  }

}
class SpecificationForSbtWithException extends Specification {
  val cause = new IllegalArgumentException("cause")
  throw new Exception("fail", cause)
  val content = "ex1" ! success ^ end
}
class SpecificationForSbt extends Specification {
  val content = "ex1" ! success ^ end
}