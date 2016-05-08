package org.specs2.runner

import org.specs2.Specification
import sbt.testing.TaskDef

class SbtRunnerSpec extends Specification { def is = s2"""

  The sbt runner can extract the tags from the specification and provide them to sbt
  if the `sbt.tags` argument is passed on the command line     $sbtTags ${tag("one")}
  if the `sbt.tags` argument is not passed on the command line $noSbtTags

"""

  def sbtTags = {
    val runner = new SbtRunner(Array("sbt.tags"), Array(""), getClass.getClassLoader)
    taskTags(runner) must contain("one")
  }

  def noSbtTags = {
    val runner = new SbtRunner(Array(""), Array(""), getClass.getClassLoader)
    taskTags(runner) must not(contain("one"))
  }

  def taskTags(runner: SbtRunner): List[String] = {
    val task = runner.newTask(new TaskDef("org.specs2.runner.SbtRunnerSpec", Fingerprints.fp1m, true, Array()))
    task.tags.toList
  }

}
