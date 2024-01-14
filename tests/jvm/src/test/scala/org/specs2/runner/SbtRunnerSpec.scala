package org.specs2.runner

import org.specs2.Specification
import org.specs2.execute.Result
import sbt.testing.TaskDef

class SbtRunnerSpec extends Specification {
  def is = s2"""

  The sbt runner can extract the tags from the specification and provide them to sbt
  if the `sbt.tags` argument is passed on the command line     $sbtTags ${tag("one")}
  if the `sbt.tags` argument is not passed on the command line $noSbtTags

"""

  def sbtTags =
    withRunnerArgs(Array("sbt.tags")) { tags => tags must contain("one") }

  def noSbtTags =
    withRunnerArgs(Array()) { tags => tags must not(contain("one")) }

  // HELPERS
  def taskTags(runner: MasterSbtRunner): List[String] =
    val task = runner.newTask(new TaskDef("org.specs2.runner.SbtRunnerSpec", Fingerprints.fp1m, true, Array()))
    task.tags.toList

  def withRunnerArgs(args: Array[String])(check: List[String] => Result): Result =
    val runner = new MasterSbtRunner(args, Array(""), getClass.getClassLoader)
    val result = check(taskTags(runner))
    runner.done
    result
}
