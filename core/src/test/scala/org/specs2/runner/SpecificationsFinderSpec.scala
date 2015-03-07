package org.specs2
package runner

import java.io.File
import io._
import FileSystem._
import matcher.ActionMatchers._
import control.Action
import matcher._
import MatchersCreation._


class SpecificationsFinderSpec extends Spec { def is = s2"""
  It is possible to find specifications in the local test directory   $e1
  It is possible to find specifications in an absolute test directory $e2
  It is possible to find specifications in a specific drive           $e3
"""

  def e1 =
    filePaths("core" / "src" / "test" / "scala", "**/*.scala", verbose = false) must findFiles

  def e2 =
    filePaths(DirectoryPath.unsafe(new File("core/src/test/scala").getAbsolutePath), "**/*.scala", verbose = false) must findFiles

  def e3 =
    filterWithPattern(globToPattern("**/*.scala"))(FilePath.unsafe(new File("T:/"+new File("core/src/test/scala/org/specs2/runner/SpecificationsFinderSpec.scala").getAbsolutePath))) must
      beTrue

  def findFiles: Matcher[Action[IndexedSeq[FilePath]]] = (action: Action[IndexedSeq[FilePath]]) =>
    action must beOk((_: IndexedSeq[FilePath]) must not(beEmpty))
}
