package org.specs2
package runner
import reporter._
import _root_.org.scalatools.testing._
/**
 * Implementation of the Framework interface for the sbt tool.
 * It declares the classes which can be executed by the specs2 library.
 */
class SpecsFramework extends Framework {
  def name = "specs2"
  trait Specs2Fingerprint extends TestFingerprint {
	def superClassName = "org.specs2.specification.BaseSpecification"
  }
  val specificationClass = new Specs2Fingerprint {
    def isModule = false
  }
  val specificationObject = new Specs2Fingerprint {
    def isModule = true
  }
  def tests = Array[Fingerprint](specificationClass, specificationObject)
  def testRunner(classLoader: ClassLoader, loggers: Array[Logger]) = {
	new TestInterfaceRunner(classLoader, loggers)
  }
}

/**
 * Runner for TestInterface.
 * It creates a Specification class with the given classLoader the classes which can be executed by the specs2 library.
 * 
 * Then it uses a NotifierRunner to notify the EventHandler of the test events.
 */
class TestInterfaceRunner(loader: ClassLoader, val loggers: Array[Logger]) extends _root_.org.scalatools.testing.Runner
  with HandlerEvents with TestLoggers {
  import reflect.Classes._
  
  def run(classname: String, fingerprint: TestFingerprint, handler: EventHandler, args: Array[String]) = {
    val specification: Either[Throwable, Specification] = create[Specification](classname + "$") match {
      case Right(s) => Right(s)
      case Left(e) => create[Specification](classname)
    }
    specification.left.map { e =>
      handler.handle(error(classname, e))
      logError("Could not create an instance of "+classname+"\n")
      logError("  "+e+"\n")
      e.getStackTrace foreach { s => logError("  "+s.toString) }
      if (e.getCause != null)
		e.getCause.getStackTrace foreach { s => logError("  "+s.toString) }
    }
    val specificationOption = specification.right.toOption
    run(specificationOption, handler)
  }
  
  private def run(specification: Option[Specification], handler: EventHandler): Option[Specification] = {
    specification map { s =>
      new TestInterfaceReporter(handler, loggers).report(s)
    }
    specification
  }
}
 