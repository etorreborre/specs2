package org.specs2
package specification
package core

import main.Arguments
import scalaz.concurrent.Task
import SpecStructure._
import scalaz.std.anyVal._
import control._
import shapeless._
import scalaz.stream.Process
import Process.{Process1, eval}

case class SpecStructure(header: SpecHeader, arguments: Arguments, fragments: Fragments) {

  def contents: Process[Task, Fragment] = contentsLens.get(this)
  def map(f: Fragments => Fragments): SpecStructure                            = fragmentsLens.modify(this)(f)
  def |>(p: Process1[Fragment, Fragment]): SpecStructure                       = fragmentsLens.modify(this)(_ |> p)
  def |>(f: Process[Task, Fragment] => Process[Task, Fragment]): SpecStructure = fragmentsLens.modify(this)(_ update f)
  def withPreviousResults(env: Env): SpecStructure                             = |>(withPreviousResult(header.className, env))

  def specClassName = header.className
  def name = header.title.getOrElse(header.simpleName)

  def texts = fragments.texts
  def examples = fragments.examples
}

object SpecStructure {
  lazy val headerLens    = lens[SpecStructure] >> 'header
  lazy val argumentsLens = lens[SpecStructure] >> 'arguments
  lazy val fragmentsLens = lens[SpecStructure] >> 'fragments
  lazy val contentsLens  = fragmentsLens >> 'contents

  def withPreviousResult(className: String, env: Env): Process[Task, Fragment] => Process[Task, Fragment] = { process =>
    if (env.arguments.wasIsDefined)
      process.flatMap { f =>
        eval(env.statisticsRepository.previousResult(className, f.description).map(r => f.setPreviousResult(r)).toTask)
      }
    else process
  }

  /** return true if s1 depends on s2, i.e, s1 has a link to s2 */
  val dependsOn = (s1: SpecStructure, s2: SpecStructure) => {
    val s1Links = s1.fragments.fragments.collect { case Fragment(l: SpecificationLink, _, _) => l.specClassName }
    s1Links.contains(s2.specClassName)
  }

}
