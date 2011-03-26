package org.specs2
package control

/**
 * This trait is used primarily to change the junit behavior depending on the execution environment
 */
private[specs2]
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
}
