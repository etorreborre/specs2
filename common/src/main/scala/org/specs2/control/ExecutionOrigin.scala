package org.specs2
package control

/** This trait is used primarily to change the junit behavior depending on the execution environment
  */
trait ExecutionOrigin extends Stacktraces:
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

  /** return true if the current test is executed with JUnitCore */
  lazy val isExecutedFromJUnitCore = isExecutedFrom("org.junit.runner")

  /** return true if the current test is executed with Scala.js */
  lazy val isExecutedFromScalaJs = isExecutedFrom("org.scalajs")

  /** return true if the current test is executed with Bazel */
  lazy val isExecutedFromBazel = isExecutedFrom("com.google.testing.junit")

  lazy val excludeFromReporting: Boolean = isExecutedFromJUnitCore || isExecutedFromBazel

  /** try to approximate if a specification is a specs2 by passing name = org.specs2 */
  def isSpecificationFromSpecs2(st: Seq[StackTraceElement]) =
    isFromClass(
      { (fullClassName: String) =>
        val className = fullClassName.takeWhile(_ != '$').mkString
        // this is a fix for #533 to properly recognize org.specs2.mutable.Spec
        // used by a normal user
        !className.endsWith(".Spec") &&
        className.endsWith("Spec") && fromSpecs2(className)
      },
      st.takeWhile(t => fromSpecs2(t.getClassName))
    )

  def fromSpecs2(className: String): Boolean =
    className.startsWith("org.specs2.")

object ExecutionOrigin extends ExecutionOrigin
