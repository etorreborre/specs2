package org.specs2
package runner

import java.io.File
import io.FileSystem._
import matcher.ControlMatchers._
import control.Action
import matcher.Matcher

class SpecificationsFinderSpec extends Specification { def is = s2"""
  It is possible to find specifications in the local test directory   $e1
  It is possible to find specifications in an absolute test directory $e2
  It is possible to find specifications in a specific drive           $e3
"""

  def e1 =
    filePaths("core/src/test/scala", "**/*.scala", verbose = false) must findFiles

  def e2 =
    filePaths(new File("core/src/test/scala").getAbsolutePath, "**/*.scala", verbose = false) must findFiles

  def e3 =
    filterFiles(Seq(new File("T:/"+new File("core/src/test/scala/org/specs2/runner/SpecificationsFinderSpec.scala").getAbsolutePath)), "**/*.scala", verbose = false) must
      not(beEmpty)

  def findFiles: Matcher[Action[Seq[String]]] = (action: Action[Seq[String]]) =>
    action must beOk((_: Seq[String]) must not(beEmpty))
}
