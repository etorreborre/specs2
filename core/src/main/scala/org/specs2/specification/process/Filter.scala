package org.specs2
package specification
package process

import scalaz.stream.{Process, process1}
import scalaz.stream.Process.{Env =>_,_}
import data._
import main._
import scalaz.syntax.foldable._
import scalaz.std.list._
import data.Processes._
import control._
import specification.core._

/**
 * Filter function for Fragment processes
 */
trait Filter {

  /** filter fragments by name, tags and previous execution */
  def filter(env: Env): Process1[Fragment, Fragment] =
    filterByName(env: Env) |> filterByTags(env) |> filterByPrevious(env)

  /** filter fragments by name */
  def filterByName(env: Env): Process1[Fragment, Fragment] = {
    val regex = env.arguments.ex
    if (regex !=".*")
      process1.filter {
        case Fragment(RawText(t),e,_) if e.isRunnable => t.matches(regex)
        case Fragment(Code(t),e,_)    if e.isRunnable => t.matches(regex)
        case other                                    => true
      }
    else process1.id
  }

  /** filter fragments by tags */
  def filterByTags(env: Env): Process1[Fragment, Fragment] = {
    val arguments = env.arguments

    def go(sections: List[NamedTag] = Nil): Process1[(Option[Fragment], Fragment, Option[Fragment]), Fragment] = {

      def updateSections(tag: NamedTag) = {
        val endTags     = sections.filter(_.names.exists(tag.names.contains))
        val startTags   = sections.map(t => t.removeNames(tag.names)).filterNot(_.names.isEmpty)
        if (endTags.isEmpty) startTags :+ tag else startTags
      }

      def filter(fragment: Fragment, tags: List[NamedTag] = sections, tag: NamedTag = AlwaysWhenNoIncludeTag): Process1[(Option[Fragment], Fragment, Option[Fragment]), Fragment] = {
        val apply = (tags :+ tag).sumr
        emit(fragment).filter(_ => apply.keep(arguments, apply.names)) fby go(tags)
      }

      receive1 {
        // tag for the next fragment
        case (Some(Fragment(Marker(t, false, true),_,_)), fragment, _) =>
          filter(fragment, tag = t)

        // start or end of a new section
        case (Some(Fragment(Marker(tag, true, true), _, _)), fragment, _) =>
          filter(fragment, updateSections(tag))

        // tag for the previous fragment
        case (_, fragment, Some(Fragment(Marker(t, false, false),_,_))) =>
          filter(fragment, tag = t)

        // start or end of a new section impacting the current fragment
        case (_, fragment, Some(Fragment(Marker(tag, true, false), _, _))) =>
          filter(fragment, updateSections(tag), tag)

        // filter fragments in between tags according to sections
        case (_, fragment, _) =>
          filter(fragment)
      }
    }

    if ((arguments.include + arguments.exclude).nonEmpty) withPreviousAndNext[Fragment] |> go()
    else process1.id[Fragment]
  }

  /**
   * filter fragments by previous execution and required status
   */
  def filterByPrevious(env: Env): Process1[Fragment, Fragment] =
    if (env.arguments.wasIsDefined) process1.filter((_: Fragment).was(env.arguments.was))
    else process1.id[Fragment]

}

object Filter extends Filter
