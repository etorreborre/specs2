package org.specs2
package control

/**
 * This trait is used primarily to change the junit behavior depending on the execution environment
 */
trait ExecutionOrigin extends Stacktraces {
  /** return true if the current test is executed with Maven */
  lazy val isExecutedFromMaven = isExecutedFrom("org.apache.maven.surefire.Surefire.run")
  /** return true if the current test is executed with sbt */
  lazy val isExecutedFromSBT = isExecutedFrom("reporter.TestInterfaceReporter")
  /** return true if the current test is executed with Gradle */
  lazy val isExecutedFromGradle = isExecutedFrom("org.gradle.api")
  /** return true if the current test is executed with eclipse */
  lazy val isExecutedFromEclipse = isExecutedFrom("org.eclipse.jdt")
  /** return true if the current test is executed with Intellij */
  lazy val isExecutedFromIntellij = isExecutedFrom("com.intellij.rt")
  /** return true if the current test is executed from an IDE */
  lazy val isExecutedFromAnIDE = isExecutedFromIntellij || isExecutedFromEclipse

  /** try to approximate if a specification is a specs2 one or scalaz one by passing name = org.specs2 or name = scalaz */
  def isSpecificationFromSpecs2orScalaz(st: Seq[StackTraceElement]) = {
    isFromClass({ fullClassName: String =>
      val className = fullClassName.takeWhile(_ != '$').mkString
      className.endsWith("Spec") && fromSpecs2orScalaz(className)
    }, st.takeWhile(t => fromSpecs2orScalaz(t.getClassName)))
  }
  def fromSpecs2orScalaz = (className: String) => className.startsWith("org.specs2.") || className.startsWith("scalaz.")
}

object ExecutionOrigin extends ExecutionOrigin